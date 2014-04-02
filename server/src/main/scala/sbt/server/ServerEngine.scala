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

final case class ExecutionId(id: Long) {
  require(id != 0L)
}

sealed trait ServerEngineWork

// If you find yourself adding stuff from sbt.protocol such as the reply serial
// to this, you are doing it wrong because from here on out MULTIPLE clients
// not just the requester care about this work, so we don't want to special-case
// the original request anymore. We also combine requests into one of these
// chunks of work, thus allRequesters not a single requester.
case class CommandExecutionWork(id: ExecutionId, command: String, allRequesters: Set[LiveClient]) extends ServerEngineWork

// this is the read-only face of TaskIdRecorder which is used outside of ServerExecuteProgress
trait TaskIdFinder {
  // guess task ID from the key and/or the current thread.
  // this returns 0 (invalid task ID) if we can't come up with any guess;
  // doesn't return an Option because normally we want to just send the event
  // anyway with a 0 task ID.
  def bestGuessTaskId(taskIfKnown: Option[protocol.ScopedKey] = None): Long

  // just look up the task ID by key, don't use any thread info.
  def taskId(task: protocol.ScopedKey): Option[Long]
}

class TaskIdRecorder extends TaskIdFinder {
  // this is not synchronized because we know we won't change it after generating it
  // in the initial ServerExecuteProgress.registered() call,
  // which (in theory) is guaranteed to be before any other threads get involved.
  // In other words it's written only from the engine thread though read
  // by many task threads.
  @volatile private var taskIds: Map[protocol.ScopedKey, Long] = Map.empty
  private var nextTaskId = 1L // start with 1 so 0 is invalid

  private object taskIdThreadLocal extends ThreadLocal[Long] {
    override def initialValue(): Long = 0
  }

  def register(key: protocol.ScopedKey): Unit = {
    taskIds += (key -> nextTaskId)
    nextTaskId += 1
  }

  def clear(): Unit = {
    taskIds = Map.empty
  }

  // TODO we want to replace this with *requiring* all event senders
  // to know their key, which means we need to pass the task key
  // through to the UIContext and the EventLogger. This can probably
  // be done with a streamsManager plus somehow relating UIContext to
  // the streams, or handling UIContext in a similar way to streams
  // where it's a dummy value replaced by sbt before invoking each
  // task. Exact details TBD and may require sbt ABI break.
  // The problem with this hack is that if a task spawns its
  // own threads, we won't have the task ID.
  def setThreadTask(key: protocol.ScopedKey): Unit =
    taskId(key) foreach { taskIdThreadLocal.set(_) }

  def clearThreadTask(): Unit =
    taskIdThreadLocal.remove()

  override def bestGuessTaskId(taskIfKnown: Option[protocol.ScopedKey] = None): Long = {
    taskIfKnown flatMap { key =>
      taskIds.get(key)
    } getOrElse taskIdThreadLocal.get
  }

  override def taskId(task: protocol.ScopedKey): Option[Long] = {
    taskIds.get(task)
  }
}

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

  private object workQueue {

    private var nextExecutionId = 1L // 1 so 0 is our invalid flag
    // as we coalesce ExecutionRequest into commands for the sbt engine
    // (combining duplicates), we store them here.
    private var workQueue: List[ServerEngineWork] = Nil
    // if we have modified workQueue since our last changed event,
    // then the value just before the first modification is here
    private var previouslyBroadcastWorkQueue: Option[List[ServerEngineWork]] = Some(Nil)

    private def savePreviousWorkQueue(): Unit = {
      previouslyBroadcastWorkQueue match {
        case Some(_) => // keep the oldest
        case None => previouslyBroadcastWorkQueue = Some(workQueue)
      }
    }

    private def emitWorkQueueChanged(state: ServerState): Unit = {
      previouslyBroadcastWorkQueue match {
        case Some(old) =>
          val oldSet = old.collect({ case command: CommandExecutionWork => command }).toSet
          val newSet = workQueue.collect({ case command: CommandExecutionWork => command }).toSet
          val added = newSet -- oldSet
          val removed = oldSet -- newSet

          for (waiting <- added)
            state.eventListeners.send(protocol.ExecutionWaiting(waiting.id.id, waiting.command))
          for (started <- removed)
            state.eventListeners.send(protocol.ExecutionStarting(started.id.id))

          // reset to nothing
          previouslyBroadcastWorkQueue = None
        case None => // we haven't made any changes
      }
    }

    private def queueNextRequest(request: ServerRequest): Unit = {
      savePreviousWorkQueue()

      workQueue = request match {
        case ServerRequest(client, serial, command: ExecutionRequest) =>
          val work: CommandExecutionWork = {
            val oldOption: Option[ServerEngineWork] = workQueue.find({
              case old: CommandExecutionWork if old.command == command.command =>
                true
              case _ =>
                false
            })

            oldOption match {
              case Some(old: CommandExecutionWork) =>
                old.copy(allRequesters = old.allRequesters + request.client)
              case None =>
                val id = ExecutionId(nextExecutionId)
                nextExecutionId += 1
                CommandExecutionWork(id, command.command, Set(request.client))
            }
          }

          import sbt.protocol.executionReceivedFormat
          request.client.reply(request.serial, ExecutionRequestReceived(id = work.id.id))

          workQueue :+ work
        case wtf =>
          throw new Error(s"we put the wrong thing in workRequestsQueue: $wtf")
      }
    }

    @tailrec
    def takeNextWork: (ServerState, ServerEngineWork) = {

      // OVERVIEW of this method.
      // 1. DO NOT BLOCK but do see if we have ServerEngineWork
      //    which can be coalesced into our workQueue.
      // 2. If workQueue has changed, send change notification
      //    events.
      // 3. If we have no workQueue after pulling off all
      //    the requests, BLOCK for the next request.
      // 4. If we DO have workQueue after pulling off all requests,
      //    pop ONE work item. Send change notification for pendingExecutions
      //    being one shorter. Return work item.

      @tailrec
      def pollRequests(): ServerState = {
        requestQueue.pollNextRequest match {
          case Right(request) =>
            queueNextRequest(request)
            pollRequests()
          case Left(state) =>
            state
        }
      }

      // NONBLOCKING scan of requests
      val serverState = pollRequests()

      // get the latest listeners before we send out the
      // work changed events; we'll also use these listeners
      // during execution of the next work item.
      eventLogger.updateClient(serverState.eventListeners)

      // Emit work queue changed here before we pop, so that
      // all work items appear in the queue once before we remove
      // them. We don't want to compress across removal.
      emitWorkQueueChanged(serverState)

      val work =
        workQueue match {
          case head :: tail =>
            savePreviousWorkQueue()
            workQueue = tail
            // then immediately notify that we've removed a queue item,
            // this way we say we've removed before we start to execute,
            // or we notify that we've emptied the queue before we block
            emitWorkQueueChanged(serverState)
            Some(head)
          case Nil =>
            None
        }

      work match {
        case Some(work) =>
          (serverState, work)
        case None =>
          // BLOCK
          val request = requestQueue.takeNextRequest
          queueNextRequest(request)
          // recurse to process the workQueue
          takeNextWork
      }
    }
  }

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
    val (serverState, work) = workQueue.takeNextWork
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
        UIShims.makeShims(state) ++
        loggingShims(state)
    // TODO - Override log manager for now, or figure out a better way.
    val extracted = Project.extract(state)
    val settings =
      SettingUtil.makeAppendSettings(rawSettings, extracted.currentRef, extracted)
    SettingUtil.reloadWithAppended(state, settings)
  }

}

// TODO - move into its own file.
// TODO - make this less hacky
// TODO - Don't hard-code all this. It should be somewhat flexible....
import xsbti._
case class FakeAppConfiguration(original: AppConfiguration, sbtVersion: String = "0.13.2-M2") extends AppConfiguration {
  final val arguments: Array[String] = Array.empty
  final def baseDirectory: File = original.baseDirectory
  private def origAp = original.provider
  object provider extends xsbti.AppProvider {
    override def scalaProvider = origAp.scalaProvider
    object id extends ApplicationID {
      override val groupID = "org.scala-sbt"
      override val name = "sbt"
      override val version = sbtVersion
      override val mainClass = "sbt.xMain"
      override val mainComponents = origAp.id.mainComponents
      override val crossVersioned = origAp.id.crossVersioned
      override val crossVersionedValue = origAp.id.crossVersionedValue
      override val classpathExtra = origAp.id.classpathExtra
    }
    override def loader: ClassLoader = origAp.loader
    override def mainClass: Class[T] forSome { type T <: AppMain } = origAp.mainClass
    override def entryPoint: Class[_] = origAp.entryPoint
    override def newMain: AppMain = origAp.newMain
    override def mainClasspath: Array[File] = origAp.mainClasspath
    override def components: ComponentProvider = origAp.components
  }
}
