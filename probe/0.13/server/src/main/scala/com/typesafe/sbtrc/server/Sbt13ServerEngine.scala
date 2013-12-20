package sbt

import com.typesafe.sbtrc.protocol._
import scala.util.control.NonFatal
import com.typesafe.sbtrc.server._
import com.typesafe.sbtrc.NeedToRebootException

/**
 * A simple sbt server engine.
 *
 *  TODO - Figure out how to handle reloading / project change detection...
 */
class Sbt13ServerEngine(private var state: State) extends AbstractSbtServerEngine {
  // For debugging only.
  val oldOut = System.out
  val oldErr = System.err

  // We want to delay doing this so that the URI is published and consumed before we
  // handle our first request and override these values.
  var hasInstalledSystemOutShims: Boolean = false

  private var running = true
  def isRunning(): Boolean = running

  override def runRequest(request: ClientRequest): Unit = {
    if(!hasInstalledSystemOutShims) {
      shims.SystemShims.replaceOutput(logStdOut, logStdErr)
      hasInstalledSystemOutShims = true
    }
    
    
    println("Request = " + request)
    val ClientRequest(client, serial, msg) = request

    // TODO - Generic return values.
    try runRequestImpl(client, serial, msg)
    catch {
      case e: NeedToRebootException =>
        running = false
        // TODO - Send this exception to all clients and don't throw.
        throw e
      case NonFatal(e) => client.send(ErrorResponse("Failure to run " + msg + ": " + e.getMessage))
      // TODO - Figure out which messages cause us to stop running....
      case e: Throwable =>
        running = false
        throw e
    }
  }

  def logStdOut(msg: String): Unit = {
    // TODO - Fire this
    LogEvent(LogStdOut(msg))
    oldOut.println(LogEvent(LogStdOut(msg)))

  }
  def logStdErr(msg: String): Unit = {
    // TODO - Fire this
    LogEvent(LogStdErr(msg))
    oldErr.println(msg)
  }

  def runRequestImpl(client: SbtClient, serial: Long, msg: Request): Unit = msg match {
    //case x: ListenToValueRequest =>
    case ExecutionRequest(command) =>
      println("Handling request for: " + command)

      val extract = Project.extract(state)
      println("Build name = " + extract.get(Keys.baseDirectory in ThisBuild))
    //case x: ExecuteRawCommand =>
    // TODO - Notifications and things.
    case _ =>
  }
}

object Sbt13ServerEngine {
  def apply(configuration: xsbti.AppConfiguration): SbtServerEngine = {
    import BasicCommands.early
    import BasicCommandStrings.runEarly
    import BuiltinCommands.{ initialize, defaults }
    import CommandStrings.{ BootCommand, DefaultsCommand, InitCommand }
    val state: State =
      StandardMain.initialState(configuration, Seq(defaults, early), runEarly(DefaultsCommand) :: runEarly(InitCommand) :: BootCommand :: Nil)
    // TODO - How do we want to trap exit calls during execution?
    // TODO - How do we report state startup errors to clients?
    // TODO - We probably have to pass the app configuration down so we can re-intialize when needed.
    new Sbt13ServerEngine(state)
  }
}