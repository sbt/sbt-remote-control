package sbt.server

import java.net.ServerSocket
import sbt.State
import sbt.server.ServerEngine
import sbt.server.SocketMessage
import sbt.protocol
import scala.util.control.NonFatal
import java.util.concurrent.TimeUnit

/**
 * This class implements the core sbt engine.   We delegate all behavior down to a single
 * threaded sbt execution engine.
 */
class SbtServer(configuration: xsbti.AppConfiguration, socket: ServerSocket) extends xsbti.Server {

  override val uri: java.net.URI = {
    val port = socket.getLocalPort
    val addr = socket.getInetAddress.getHostAddress
    new java.net.URI(s"http://${addr}:${port}")
  }

  val masterLogFile = new java.io.File(configuration.baseDirectory, ".sbtserver/master.log")
  if (!masterLogFile.getParentFile.mkdirs())
    System.err.println(s"Could not create directory ${masterLogFile.getParentFile}")
  val masterLog = FileLogger(masterLogFile)

  // The queue where requests go before we fullfill them.
  private val queue = new java.util.concurrent.LinkedBlockingDeque[SocketMessage]

  private val stateRef = new java.util.concurrent.atomic.AtomicReference[State](null)
  private val requestProcessor = new sbt.server.RequestProcessor(queue, stateRef)
  private val commandEngine = new sbt.server.ServerEngine(requestProcessor.engineWorkQueue, stateRef, masterLog,
    // this is a little silly but keeping the ability to break them up later
    requestProcessor.eventSink, requestProcessor.eventSink, requestProcessor.eventSink, requestProcessor.eventSink)

  // External API to run queue.
  def queueSocketMessage(request: SocketMessage): Unit = queue.add(request)

  // Create the helper which will handle socket requests.
  private val socketHandler = new SbtServerSocketHandler(socket, queueSocketMessage, masterLogFile)

  // TODO - Maybe the command engine should extend thread too?
  val commandEngineThreadResult = concurrent.Promise[xsbti.MainResult]()
  private val commandEngineThread = new Thread("sbt-server-command-loop") {
    override def run(): Unit = {
      try {
        val originOut = System.out
        val originErr = System.err
        // TODO - Timeouts that lead to us shutting down the server.
        val result = try Right(commandEngine.execute(configuration))
        catch {
          case e: xsbti.FullReload =>
            // this means we want to reboot; we can kick it up
            // to another thread via commandEngineThreadResult
            Left(e)
        }
        masterLog.log(s"Done executing sbt server engine, result $result")
        result match {
          case Left(e) => commandEngineThreadResult.failure(e)
          case Right(r) => commandEngineThreadResult.success(r)
        }
      } catch {
        case t: Throwable =>
          masterLog.error(s"command engine thread crash ${t.getClass.getName}: ${t.getMessage}", t)
          commandEngineThreadResult.tryFailure(new Exception("command engine thread crashed", t))
      }
    }
  }
  override def awaitTermination(): xsbti.MainResult = try {
    // Log some handy debug info
    val mx = java.lang.management.ManagementFactory.getRuntimeMXBean();
    masterLog.log(s"jvm ${mx.getName()}")
    masterLog.log(s"jvm vmName=${mx.getVmName} vmVendor=${mx.getVmVendor} vmVersion=${mx.getVmVersion}")
    masterLog.log(s"jvm specName=${mx.getSpecName} specVendor=${mx.getSpecVendor} specVersion=${mx.getSpecVersion}")
    masterLog.log(s"jvm arguments: ${mx.getInputArguments()}")

    // Here we actually start.
    masterLog.log("Starting event engine")
    requestProcessor.start()

    masterLog.log("Starting sbt command engine")
    commandEngineThread.start()

    // Wait for the server to stop (which means: no more sbt commands in State).
    // If requestProcessor stops first, it will send a command to the command
    // engine asking the command engine to stop.
    masterLog.log("Waiting for sbt command engine")
    commandEngineThread.join()

    // Close down the socket handler which should signal requestProcessor to stop
    // if it hasn't already. requestProcessor drains all requests then stops, if the
    // socket handler has stopped.
    masterLog.log("Closing listening server socket")
    socketHandler.stop()

    masterLog.log("Waiting for socket thread")
    socketHandler.join()

    masterLog.log("Waiting for request processor thread")
    requestProcessor.join()

    masterLog.log("Waiting for command engine result")

    // If this throws FullReload, then the entire server machinery should restart including
    // a new socket, in theory.
    val result = concurrent.Await.result(commandEngineThreadResult.future, concurrent.duration.Duration(2, TimeUnit.SECONDS))

    masterLog.log(s"Returning control to sbt launcher with result $result")

    result
  } catch {
    case e: xsbti.FullReload =>
      // this exception tells the launcher code to reload
      masterLog.log(s"Throwing FullReload up to sbt launcher")
      throw e
    case NonFatal(e) =>
      masterLog.error(s"Unexpected error ${e.getClass.getName}: ${e.getMessage}", e)
      Exit(1)
  } finally {
    masterLog.close()
  }
}
