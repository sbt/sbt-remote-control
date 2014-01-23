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
import sbt.StandardMain
import sbt.State

case class ServerRequest(client: LiveClient, serial: Long, request: Request)

/** An abstract implementation of the sbt server engine that can be used by clients. */
abstract class ServerEngine {

  /**
   * This blocks the current thread looking for the next request to execute and returns it,
   * or throws an exception if we're done.
   */
  def takeNextRequest: ServerRequest
  /**
   * This is used to grab all event listening requests in one blow when
   *  we first start up, as most clients will send one immediately.
   *
   *  This should clean the queue of all event listening requests if possible.
   */
  def takeAllEventListenerRequests: Seq[ServerRequest]

  // Set up the server with all the initial things we need to do.
  final val InitializeServerState = "server-initialize-state"
  final def initializeServerStateCommand = Command.command(InitializeServerState) { state =>
    val eventListeners = takeAllEventListenerRequests collect {
      case ServerRequest(client, _, ListenToEvents()) => client
    }
    val ss = (ServerState() /: eventListeners) { _ addEventListener _ }
    System.out.println("Initial listeners = " + eventListeners)
    ServerState.update(state, ss)
    // TODO - Should we check to see if anyone is listening to build state yet?

    // Here we want to register our error handler that handles build load failure.
  }

  // A command which runs after sbt has loaded and we're ready to handle requests.
  final val SendReadyForRequests = "server-send-ready-for-request"
  final def sendReadyForRequests = Command.command(SendReadyForRequests) { state =>
    // Notify everyone we're ready to start...
    val serverState = ServerState.extract(state)
    serverState.eventListeners.send(NowListeningEvent)

    // here we want to register our error handler that handles command failure.
    installBuildHooks(state.copy(onFailure = Some(PostCommandErrorHandler)))
  }
  final val HandleNextServerRequest = "server-handle-next-server-request"
  // TODO - Clean this guy up a bit
  final def handleNextRequestCommand = Command.command(HandleNextServerRequest) { state =>
    // TODO - What todo with failure?  Maybe we just wait for the next request...
    val ServerRequest(client, serial, request) = takeNextRequest
    val next = handleRequest(client, serial, request, state)
    // make sure we always read another server request after this one
    // TODO - CHeck to see if we're done...
    next.copy(remainingCommands = next.remainingCommands :+ PostCommandCleanup :+ HandleNextServerRequest)
  }

  final val PostCommandCleanup = "server-post-command-cleanup"
  final def postCommandCleanupCommand = Command.command(PostCommandCleanup)(postCommandCleanup)
  // TODO - Maybe this should be its own command...
  // TODO - Maybe this should be called cleanupLastCommand?
  def postCommandCleanup(state: State): State = {
    val serverState = ServerState.extract(state)
    serverState.lastCommand match {
      case Some(command) =>
        serverState.eventListeners.send(ExecutionDone(command.command))
        command.client.reply(command.serial, RequestCompleted())
      // TODO - notify original requester we're done?
      case None => ()
    }
    ServerState.update(state, serverState.clearLastCommand)
  }

  final def PostCommandErrorHandler = "server-post-command-error-handler"
  final def postCommandErrorHandler = Command.command(PostCommandErrorHandler) { state: State =>
    // TODO - Is this error going to be useful?
    val lastState = ServerState.extract(state)
    lastState.lastCommand match {
      case Some(LastCommand(command, serial, client)) =>
        client.reply(serial, ErrorResponse("Unknown failure while running command: " + command))
      // TODO - Should we be replacing the PostCommandCleanup stuff here?
      case None => ()
    }
    PostCommandCleanup :: HandleNextServerRequest :: state
  }

  val henrikIsAwesome = taskKey[Boolean]("")

  def handleRequest(client: LiveClient, serial: Long, request: Request, state: State): State = {
    val serverState = ServerState.extract(state)
    request match {
      case ListenToEvents() =>
        // Immediately tell new clients we're ready to receive commands if they're listening
        // for events.
        client.send(NowListeningEvent)
        ServerState.update(state, serverState.addEventListener(client))
      // TODO - update global logging?
      // TODO - Ack the server?
      case ListenToBuildChange() =>
        // We should immediately send the structure...
        sendBuildStructure(state, client)
        // And add the client to get further notifications.
        ServerState.update(state, serverState.addBuildListener(client))
      case ClientClosedRequest() =>
        val serverState = ServerState.extract(state)
        ServerState.update(state, serverState.disconnect(client))
      case ExecutionRequest(command) =>
        // TODO - Figure out how to run this and ack appropriately...
        command :: ServerState.update(state, serverState.withLastCommand(LastCommand(command, serial, client)))
      case ListenToValue(key) =>
        // TODO - We also need to get the value to send to the client...
        //  This only registers the listener, but doesn't actually 
        SbtToProtocolUtils.protocolToScopedKey(key, state) match {
          case Some(key) =>
            // Schedule the key to run as well as registering the key listener. 
            val extracted = Project.extract(state)
            extracted.showKey(key) :: ServerState.update(state, serverState.addKeyListener(client, key))
          case None => // Issue a no such key error
            client.reply(serial, ErrorResponse(s"Unable to find key: $key"))
            state
        }
      // TODO - This may need to be in an off-band channel. We should be able to respond
      // to these requests DURING another task execution....
      case CommandCompletionsRequest(id, line, level) =>
        val combined = state.combinedParser
        val completions = complete.Parser.completions(combined, line, level)
        def convertCompletion(c: complete.Completion): protocol.Completion =
          protocol.Completion(
            c.append,
            c.display,
            c.isEmpty)
        client.send(CommandCompletionsResponse(id, completions.get map convertCompletion))
        state
    }
  }

  def sendBuildStructure(state: State, listener: SbtClient): Unit = {
    val structure = SbtDiscovery.buildStructure(state)
    listener.send(BuildStructureChanged(structure))
  }

  /** This will load/launch the sbt execution engine. */
  protected def execute(configuration: xsbti.AppConfiguration): Unit = {
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
          initializeServerStateCommand,
          handleNextRequestCommand,
          sendReadyForRequests,
          postCommandCleanupCommand,
          postCommandErrorHandler) ++ BuiltinCommands.DefaultCommands,
        // Note: We drop the default command in favor of just adding them to the state directly.
        // TODO - Should we try to handle listener requests before booting?
        preCommands = runEarly(InitCommand) :: runEarly(InitializeServerState) :: BootCommand :: SendReadyForRequests :: HandleNextServerRequest :: Nil)

    // Now feed the state through an appropriate engine.   For now, we'll use the standard main...
    // TODO - We need to write our own command loop
    // that will inspect server state and issue appropriate commands.
    MainLoop.runLogged(state)
  }

  /** Can convert an event logger into GlobalLogging. */
  def initializeLoggers(serverLogFile: File): GlobalLogging = {
    // First override all logging.
    serverLogFile.getParentFile.mkdirs()
    val output = new java.io.PrintWriter(new java.io.FileWriter(serverLogFile))
    EventLogger.updatePeer({ msg => output.println(msg); output.flush })
    def handleStdOut(line: String): Unit = EventLogger.send(LogStdOut(line))
    def handleStdErr(line: String): Unit = EventLogger.send(LogStdErr(line))
    SystemShims.replaceOutput(handleStdOut, handleStdErr)
    def throwawayBackingFile = java.io.File.createTempFile("sbt-server-", ".log")
    def newBacking =
      GlobalLogBacking(file = throwawayBackingFile,
        last = None,
        newBackingFile = () => throwawayBackingFile)
    def globalLogging: GlobalLogging =
      GlobalLogging(
        full = EventLogger,
        console = EventLogger.consoleOut,
        backed = ConsoleLogger(EventLogger.consoleOut),
        backing = newBacking,
        newLogger = (writer, oldBacking) => globalLogging)
    globalLogging
  }

  // Here we install our basic build hooks we use to fire events.
  def installBuildHooks(state: State): State = {
    val rawSettings: Seq[Setting[_]] =
      TestShims.makeShims(state) ++
        CompileReporter.makeShims(state) ++
        ServerExecuteProgress.getShims(state) ++
        UIShims.makeShims(state)
    val extracted = Project.extract(state)
    val settings =
      SettingUtil.makeAppendSettings(rawSettings, extracted.currentRef, extracted)
    SettingUtil.reloadWithAppended(state, settings)
  }

}

// TODO - move into its own file.
// TODO - make this less hacky
import xsbti._
case class FakeAppConfiguration(original: AppConfiguration, sbtVersion: String = "0.13.2-M1") extends AppConfiguration {
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