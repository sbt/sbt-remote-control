package sbt.server

import sbt.impl.ipc
import ipc.{ MultiClientServer => IpcServer }
import sbt.protocol.{ Envelope, Message, Request, ConfirmRequest, ConfirmResponse, ReadLineRequest, ReadLineResponse, ErrorResponse }
import play.api.libs.json.Writes
import concurrent.{ Promise, promise }
import java.io.EOFException
import java.net.SocketException
import scala.util.control.NonFatal

/**
 * This class represents an external client into the sbt server.
 *
 * We forward messages from the client into the sbt build loop.
 */
class SbtClientHandler(
  override val uuid: java.util.UUID,
  override val configName: String,
  override val humanReadableName: String,
  ipc: IpcServer,
  msgHandler: ServerRequest => Unit,
  closed: () => Unit) extends sbt.server.LiveClient {

  // TODO - Configure this location.
  // TODO - Is this thread safe-ish?
  private val log = FileLogger(new java.io.File(s".sbtserver/connections/${configName}-${uuid}.log"))

  // If plugins/tasks can ever *receive* custom build values in some kind of
  // request-to-task or task-to-task event, we would probably want to store the JsValue
  // in the value to deserialize here, and then in ServerEngine just before processing
  // the message in question, grab our registered deserializers off of State and
  // deserialize the embedded JsValue. But for now this never happens so we can just do this.
  private val serializations = DynamicSerialization.defaultSerializations

  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  def isAlive: Boolean = clientThread.isAlive && running.get
  private object clientThread extends Thread(s"sbt-client-handler-$configName-$uuid") {
    final override def run(): Unit = {
      import sbt.protocol
      send(protocol.CoreLogEvent(protocol.LogMessage(protocol.LogMessage.DEBUG,
        s"sbt client logs are in: ${log.file.getAbsolutePath}")))
      while (running.get) {
        try readNextMessage()
        catch {
          case e: EOFException =>
            log.log(s"Client $configName-$uuid EOF, shutting down.")
            running.set(false)
          case e: SocketException =>
            log.log(s"Client $configName-$uuid closed, ${e.getClass.getName}: ${e.getMessage}, shutting down")
            running.set(false)
          case e: Throwable =>
            // On any throwable, we'll shut down this connection as bad.
            log.error(s"Client $configName-$uuid had error, shutting down", e)
            // TODO - Remove this.
            e.printStackTrace(System.err)
            running.set(false)
        }
      }
      if (!ipc.isClosed) {
        log.log(s"Stopping client.")

        // It's ok to close this connection when we're done.
        ipc.close()
      }
      // Here we send a client disconnected message to the main sbt
      // engine so it stops using this client.
      msgHandler(ServerRequest(SbtClientHandler.this, 0L, sbt.protocol.ClientClosedRequest()))
      // Here we tell the server thread handler... this MUST be in another thread
      // or it could deadlock when someone is trying to join().
      concurrent.Future(closed())(concurrent.ExecutionContext.Implicits.global)
      log.log(s"Client $configName-$uuid thread exiting.")
    }
    private def readNextMessage(): Unit = {
      log.log("Reading next message from client.")
      Envelope(ipc.receive()) match {
        case Envelope(serial, replyTo, msg: Request) =>
          log.log(s"Got request: $msg")
          reply(serial, sbt.protocol.ReceivedResponse())
          val request = ServerRequest(SbtClientHandler.this, serial, msg)
          msgHandler(request)
        case Envelope(_, replyTo, msg: ConfirmResponse) =>
          log.log(s"Response: $replyTo - $msg")
          interactionManager.confirmed(replyTo, msg.confirmed)
        case Envelope(_, replyTo, msg: ReadLineResponse) =>
          log.log(s"Response: $replyTo - $msg")
          interactionManager.lineRead(replyTo, msg.line)
        case Envelope(_, replyTo, msg: ErrorResponse) =>
          // TODO - other notifications?
          log.log(s"Response: $replyTo - $msg")
          interactionManager.error(replyTo, msg.error)
        case Envelope(_, _, msg) =>
          sys.error("Unable to handle client request: " + msg)
      }
    }
  }
  // Automatically start listening for client events.
  clientThread.start()

  private def wrappedSend(msg: Any)(block: => Unit): Unit = {
    if (isAlive) {
      log.log(s"Sending msg to client $configName-$uuid: $msg")
      try block
      catch {
        case NonFatal(e) =>
          log.log(s"Dropping message ${msg} because client $configName-$uuid appears to be closed ${e.getClass.getName}: ${e.getMessage}")
          // double-ensure close, in case Java wants to be annoying and not terminate our read()
          ipc.close()
      }
    } else {
      log.log(s"Dropping message ${msg} to dead client $configName-$uuid")
    }
  }

  // ipc is synchronized, so this is ok.
  override def send[T <: Message: Writes](msg: T): Unit = {
    wrappedSend(msg) { ipc.sendJson(msg, ipc.serialGetAndIncrement()) }
  }
  // ipc is synchronized, so this is ok.
  override def reply[T: Writes](serial: Long, msg: T): Unit = {
    wrappedSend(serial -> msg) { ipc.replyJson(serial, msg) }
  }
  def readLine(executionId: ExecutionId, prompt: String, mask: Boolean): concurrent.Future[Option[String]] =
    interactionManager.readLine(executionId, prompt, mask)
  def confirm(executionId: ExecutionId, msg: String): concurrent.Future[Boolean] =
    interactionManager.confirm(executionId, msg)

  object interactionManager {
    private var readLineRequests: Map[Long, Promise[Option[String]]] = Map.empty
    private var confirmRequests: Map[Long, Promise[Boolean]] = Map.empty

    // TODO - timeouts on requests....

    def error(serial: Long, msg: String): Unit = synchronized {
      (readLineRequests get serial) orElse (confirmRequests get serial) match {
        // TODO - Custom exception.
        case Some(x: Promise[_]) =>
          x.failure(new RuntimeException(msg))
          // Now clean out the request handler
          readLineRequests -= serial
          confirmRequests -= serial
        case None => // TODO - error
      }
    }
    def readLine(executionId: ExecutionId, prompt: String, mask: Boolean): concurrent.Future[Option[String]] =
      synchronized {
        val result = Promise[Option[String]]()
        val newSerial = ipc.serialGetAndIncrement()
        readLineRequests += newSerial -> result
        ipc.sendJson(ReadLineRequest(executionId.id, prompt, mask), newSerial)
        result.future
      }
    def lineRead(serial: Long, line: Option[String]): Unit =
      synchronized {
        readLineRequests get serial match {
          case Some(promise) =>
            promise.success(line)
            // Now clean out the request handler
            readLineRequests -= serial
          case None => // TODO - log error?
        }
      }
    def confirm(executionId: ExecutionId, msg: String): concurrent.Future[Boolean] =
      synchronized {
        val result = Promise[Boolean]()
        val newSerial = ipc.serialGetAndIncrement()
        confirmRequests += newSerial -> result
        ipc.sendJson(ConfirmRequest(executionId.id, msg), newSerial)
        result.future
      }

    def confirmed(serial: Long, value: Boolean): Unit =
      synchronized {
        confirmRequests get serial match {
          case Some(promise) =>
            promise.success(value)
            // Now clean out the request handler
            confirmRequests -= serial
          case None => // TODO - log error?
        }
      }
  }

  def shutdown(): Unit = {
    running.set(false)
    // otherwise we won't wake up until we get a request
    try ipc.close() catch { case NonFatal(e) => }
  }
  def join(): Unit = {
    clientThread.join()
    log.close()
  }

  override def equals(o: Any): Boolean =
    o match {
      case x: SbtClientHandler => uuid == x.uuid
      case _ => false
    }
  override def hashCode = uuid.hashCode
  override def toString = "LiveClient(" + configName + " " + uuid + ")"
}
