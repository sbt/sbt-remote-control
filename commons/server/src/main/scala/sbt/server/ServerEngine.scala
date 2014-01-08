package sbt
package server

import com.typesafe.sbtrc.protocol._
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
case class ServerRequest(client: SbtClient, request: Request)

/** An abstract implementation of the sbt server engine that can be used by clients. */
abstract class ServerEngine {
  
  /** This blocks the current thread looking for the next request to execute and returns it,
   * or throws an exception if we're done.
   */
  def takeNextRequest: ServerRequest
  
  
  final val InitializeServerState = "initialize-server-state"
  final def initializeServerStateCommand = Command.command(InitializeServerState) { state =>
    System.out.println("DEBUGME: server state initialized.")
    ServerState.update(state, ServerState())
  }
  final val HandleNextServerRequest = "handle-next-server-request"
  // TODO - Clean this guy up a bit
  final def handleNextRequestCommand = Command.command(HandleNextServerRequest) { state =>
    // TODO - What todo with failure?  Maybe we just wait for the next request...
    val ServerRequest(client, request) = takeNextRequest
    val next = handleRequest(client, request, state)
    // make sure we always read another server request after this one
    // TODO - CHeck to see if we're done...
    next.copy(remainingCommands = next.remainingCommands :+ HandleNextServerRequest)
  }
  
  def handleRequest(client: SbtClient, request: Request, state: State): State = {
    System.out.println("DEBUGME: Reading server requests!")
    val serverState = ServerState.extract(state) 
    request match {
      case ListenToEvents() => 
        ServerState.update(state, serverState.addEventListener(client))
        // TODO - update global logging?
        // TODO - Ack the server?
      case ClientClosedRequest() =>
        val serverState = ServerState.extract(state)
        ServerState.update(state, serverState.disconnect(client))
      case ExecutionRequest(command) =>
        // TODO - Figure out how to run this and ack appropriately...
        command :: state
    }   
  }
  
  /** This will load/launch the sbt execution engine. */
  protected def execute(configuration: xsbti.AppConfiguration): Unit = {
    import BasicCommands.early
    import BasicCommandStrings.runEarly
    import BuiltinCommands.{ initialize, defaults }
    import CommandStrings.{ BootCommand, DefaultsCommand, InitCommand }

   // TODO - can this be part of a command?
    System.out.println("Installing loggers.")
    val globalLogging = initializeLoggers(new File(configuration.baseDirectory, ".sbtserver/master.log"))
    // TODO - This is copied from StandardMain so we can override globalLogging
    def initialState(configuration: xsbti.AppConfiguration, initialDefinitions: Seq[Command], preCommands: Seq[String]): State =
	{
		import BasicCommandStrings.isEarlyCommand
		//val userCommands = configuration.arguments.map(_.trim)
		val (earlyCommands, normalCommands) = (preCommands /*++ userCommands*/).partition(isEarlyCommand)
		val commands = earlyCommands ++ normalCommands
		val initAttrs = BuiltinCommands.initialAttributes
		val s = State( configuration, initialDefinitions, Set.empty, None, commands, State.newHistory, initAttrs, globalLogging, State.Continue )
		s.initializeClassLoaderCache
	}
    val state: State =
      initialState(configuration, 
          initialDefinitions = Seq(early, initializeServerStateCommand, handleNextRequestCommand) ++ BuiltinCommands.DefaultCommands,
          // Note: We drop the default command in favor of just adding them to the state directly.
          preCommands = runEarly(InitCommand) :: runEarly(InitializeServerState) :: BootCommand  :: HandleNextServerRequest :: Nil)

    
    System.out.println("DEBUGME: Starting sbt main loop!")
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
    EventLogger.updatePeer({msg => output.println(msg ); output.flush})
    import com.typesafe.sbtrc.protocol._
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
  

}