package sbt

import com.typesafe.sbtrc.protocol._
import scala.util.control.NonFatal
import com.typesafe.sbtrc.server._
import com.typesafe.sbtrc.NeedToRebootException
import com.typesafe.sbtrc.ipc.JsonWriter

/**
 * A simple sbt server engine.
 *
 *  TODO - Figure out how to handle reloading / project change detection...
 */
class Sbt13ServerEngine(buildState: State) extends AbstractSbtServerEngine {
  // For debugging only.
  val oldOut = System.out
  val oldErr = System.err

  val logFile = new File(".sbtserver.log")

  val logStream = new java.io.PrintWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(logFile)))

  // This should be the only mutable variable in the whole engine.
  // We expose this so that clients/things running on other threads can fire 
  // notifications to the latest listeners.
  @volatile
  private var state: SbtServerState = SbtServerState(buildState)

  // TODO - if we're volatile and immutable, are we still ok?
  def sendEvent[T: JsonWriter](msg: T): Unit = {
    state.eventListeners.send(msg)
  }

  // TODO - This junk should not be mutable. Everything we mutate should be in the build state.
  // We want to delay doing this so that the URI is published and consumed before we
  // handle our first request and override these values.
  var hasInstalledSystemOutShims: Boolean = false

  // This is only accessed on one thread, we think...
  private var running = true
  def isRunning(): Boolean = running

  override def runRequest(request: ClientRequest): Unit = {
    if (!hasInstalledSystemOutShims) {
      shims.SystemShims.replaceOutput(logStdOut, logStdErr)
      hasInstalledSystemOutShims = true
    }

    System.out.println("Request = " + request)
    val ClientRequest(client, serial, msg) = request

    // TODO - Generic return values.
    try {
      state = runRequestImpl(client, serial, msg, state)
    } catch {
      case e: NeedToRebootException =>
        running = false
        // TODO - Send this exception to all clients and don't throw.
        throw e
      case NonFatal(e) =>
        client.send(ErrorResponse("Failure to run " + msg + ": " + e.getMessage))
        e.printStackTrace(oldErr)
      // TODO - Figure out which messages cause us to stop running....
      // Or any other cleanup...
      case e: Throwable =>
        running = false
        throw e
    }
  }

  def logStdOut(msg: String): Unit = {
    // TODO - Should we be double writing these things?
    sendEvent(LogEvent(LogStdOut(msg)))
    logStream.print(msg)
    logStream.flush()
  }
  def logStdErr(msg: String): Unit = {
    // TODO - Should we be double writing these things?
    sendEvent(LogEvent(LogStdErr(msg)))
    logStream.print(msg)
    logStream.flush()
  }

  // TODO - Clean this up a bit so it's more obvious what happens.  This method is the
  // core behind our engine.  take in a new request, and feed back the next state.
  def runRequestImpl(client: SbtClient, serial: Long, msg: Request, state: SbtServerState): SbtServerState = msg match {
    case ClientClosedRequest() =>
      // TODO - remove all listeners and state from this client, not just event listeners.
      state.disconnect(client)
    case ListenToEvents() =>
      System.out.println("Registering listener: " + client)
      state.copy(eventListeners = state.eventListeners zip client)
    // TODO - return ACK
    case ExecutionRequest(command) =>
      System.out.println("Handling request for: " + command)
      val extract = Project.extract(state.buildState)
      System.out.println("Build name = " + extract.get(Keys.baseDirectory in ThisBuild))
      state
    case _ => state
  }
}

object Sbt13ServerEngine {
  def apply(configuration: xsbti.AppConfiguration): SbtServerEngine = {
    import BasicCommands.early
    import BasicCommandStrings.runEarly
    import BuiltinCommands.{ initialize, defaults }
    import CommandStrings.{ BootCommand, DefaultsCommand, InitCommand }
    // TODO - we still don't have an initialized session here	
    val state: State =
      StandardMain.initialState(configuration, Seq(defaults, early), runEarly(DefaultsCommand) :: runEarly(InitCommand) :: BootCommand :: Nil)
    // TODO - How do we want to trap exit calls during execution?
    // TODO - How do we report state startup errors to clients?
    // TODO - We probably have to pass the app configuration down so we can re-intialize when needed.
    new Sbt13ServerEngine(state)
  }
}