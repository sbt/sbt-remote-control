package sbt
package server

import sbt.protocol._
import BasicCommandStrings.runEarly
import BasicCommands.early
import BuiltinCommands.defaults
import CommandStrings.BootCommand
import CommandStrings.DefaultsCommand
import CommandStrings.InitCommand
import sbt.Command
import sbt.MainLoop
import sbt.State
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.{ Future, Promise }

/**
 * An implementation of the sbt command server engine that can be used by clients.  This makes no
 *  assumptions about the implementation of handling sockets, etc.  It only requires a queue from which
 *  it can draw events *and* a thread it will own during the lifecycle of event processing.
 *
 *  @param nextStateRef - We dump the last computed state for read-only processing once it's ready for consumption.
 *  @param queue - The queue we consume server requests from.
 */
class ServerEngine(requestQueue: ServerEngineQueue, nextStateRef: AtomicReference[State]) {

  private val taskIdRecorder = new TaskIdRecorder
  private val eventLogger = new EventLogger(taskIdRecorder)

  // A command which runs after sbt has loaded and we're ready to handle requests.
  final val SendReadyForRequests = "server-send-ready-for-request"
  final def sendReadyForRequests = Command.command(SendReadyForRequests) { state =>
    // here we want to register our error handler that handles command failure.
    val newState = installBuildHooks(state.copy(onFailure = Some(PostCommandErrorHandler)))
    // Notify server event loop of the build state
    nextStateRef.lazySet(newState)
    newState
  }

  final val HandleNextServerRequest = "server-handle-next-server-request"
  final def handleNextRequestCommand = Command.command(HandleNextServerRequest) { state =>
    val (serverState, work) = requestQueue.blockAndTakeNext
    // Update server state on stdout/logger for this request.
    eventLogger.updateClient(serverState.eventListeners)
    // here we inject the current serverState into the State object.
    val next = handleWork(work, ServerState.update(state, serverState))
    // make sure we always read another server request after this one
    // TODO - CHeck to see if we're done...
    next.copy(remainingCommands = next.remainingCommands :+ PostCommandCleanup :+ HandleNextServerRequest)
  }

  final val PostCommandCleanup = "server-post-command-cleanup"
  final def postCommandCleanupCommand = Command.command(PostCommandCleanup)(postCommandCleanup)
  def postCommandCleanup(state: State): State = {
    // Make sure we update the reference to state for read-only
    // stuffs before handling our next request.
    nextStateRef.lazySet(state)
    val serverState = ServerState.extract(state)
    val nextState = serverState.lastCommand match {
      case Some(command) =>
        serverState.eventListeners.send(ExecutionSuccess(command.command.id.id))
        command.command.cancelStatus.complete()
        BuildStructureCache.update(state)
      case None => state
    }
    ServerState.update(nextState, serverState.clearLastCommand)
  }

  final def PostCommandErrorHandler = "server-post-command-error-handler"
  final def postCommandErrorHandler = Command.command(PostCommandErrorHandler) { state: State =>
    val lastState = ServerState.extract(state)
    lastState.lastCommand match {
      case Some(LastCommand(command)) =>
        command.cancelStatus.complete()
        lastState.eventListeners.send(ExecutionFailure(command.id.id))
      case None => ()
    }
    // NOTE - we always need to re-register ourselves as the error handler.
    val withErrorHandler = state.copy(onFailure = Some(PostCommandErrorHandler))
    // Here we clear the last command so we don't report success in the next step.
    val clearedCommand = ServerState.update(withErrorHandler, lastState.clearLastCommand)
    PostCommandCleanup :: HandleNextServerRequest :: clearedCommand
  }

  def handleWork(work: ServerEngineWork, state: State): State = {
    val serverState = ServerState.extract(state)
    work match {
      case work: CommandExecutionWork =>
        // TODO - Figure out how to run this and ack appropriately...
        work.command :: ServerState.update(state, serverState.withLastCommand(LastCommand(work)))
      case other =>
        // TODO - better error reporting here!
        sys.error("Command loop unable to handle msg: " + other)
    }
  }

  /** This will load/launch the sbt execution engine. */
  def execute(configuration: xsbti.AppConfiguration): Unit = {
    import BasicCommands.early
    import BasicCommandStrings.runEarly
    import BuiltinCommands.{ initialize, defaults }
    import CommandStrings.{ BootCommand, DefaultsCommand, InitCommand }

    // TODO - can this be part of a command?
    val globalLogging = initializeLoggers(new File(configuration.baseDirectory, ".sbtserver/master.log"))
    // TODO - This is copied from StandardMain so we can override globalLogging
    def initialState(configuration: xsbti.AppConfiguration, initialDefinitions: Seq[Command], preCommands: Seq[String]): State =
      {
        import BasicCommandStrings.isEarlyCommand
        //val userCommands = configuration.arguments.map(_.trim)
        val (earlyCommands, normalCommands) = (preCommands /*++ userCommands*/ ).partition(isEarlyCommand)
        val commands = earlyCommands ++ normalCommands
        val initAttrs = BuiltinCommands.initialAttributes
        val s = State(configuration, initialDefinitions, Set.empty, None, commands, State.newHistory, initAttrs, globalLogging, State.Continue)
        s.initializeClassLoaderCache
      }
    val state: State =
      // TODO - We need the sbt version to create the fake configuration.
      initialState(FakeAppConfiguration(configuration),
        initialDefinitions = Seq(
          early,
          handleNextRequestCommand,
          sendReadyForRequests,
          postCommandCleanupCommand,
          postCommandErrorHandler) ++ BuiltinCommands.DefaultCommands,
        // Note: We drop the default command in favor of just adding them to the state directly.
        // TODO - Should we try to handle listener requests before booting?
        preCommands = runEarly(InitCommand) :: BootCommand :: SendReadyForRequests :: HandleNextServerRequest :: Nil)
    StandardMain.runManaged(state)
  }

  /** Can convert an event logger into GlobalLogging. */
  def initializeLoggers(serverLogFile: File): GlobalLogging = {
    // First override all logging.
    serverLogFile.getParentFile.mkdirs()
    val output = new java.io.PrintWriter(new java.io.FileWriter(serverLogFile))
    eventLogger.updatePeer({ msg => output.println(msg); output.flush })
    def handleStdOut(line: String): Unit = eventLogger.send(LogStdOut(line))
    def handleStdErr(line: String): Unit = eventLogger.send(LogStdErr(line))
    SystemShims.replaceOutput(handleStdOut, handleStdErr)
    def throwawayBackingFile = java.io.File.createTempFile("sbt-server-", ".log")
    def newBacking =
      GlobalLogBacking(file = throwawayBackingFile,
        last = None,
        newBackingFile = () => throwawayBackingFile)
    def globalLogging(backing: GlobalLogBacking): GlobalLogging =
      GlobalLogging(
        full = eventLogger, // TODO - Send this to the "writer" we get in newLogger.
        console = eventLogger.consoleOut,
        backed = eventLogger,
        backing = backing,
        // TODO - This needs to be fixed.  Does not use the correct writer to enable "last" to work properly.
        newLogger = (writer, newBacking) => globalLogging(newBacking))
    globalLogging(newBacking)
  }

  def loggingShims(state: State): Seq[Setting[_]] = {
    val extracted = Project.extract(state)
    LogManager.defaults(null, null)
    for {
      project <- extracted.structure.allProjectRefs
    } yield Keys.logManager in project := LogManager.withLoggers()
  }

  // Here we install our basic build hooks we use to fire events.
  def installBuildHooks(state: State): State = {
    val rawSettings: Seq[Setting[_]] =
      TestShims.makeShims(state) ++
        CompileReporter.makeShims(state) ++
        ServerExecuteProgress.getShims(state, taskIdRecorder) ++
        UIShims.makeShims(state, taskIdRecorder) ++
        loggingShims(state) ++
        ServerTaskCancellation.getShims()
    // TODO - Override log manager for now, or figure out a better way.
    val extracted = Project.extract(state)
    val settings =
      SettingUtil.makeAppendSettings(rawSettings, extracted.currentRef, extracted)
    SettingUtil.reloadWithAppended(state, settings)
  }
}

