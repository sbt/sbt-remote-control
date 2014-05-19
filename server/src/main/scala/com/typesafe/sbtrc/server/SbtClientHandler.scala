package com.typesafe.sbtrc
package server

import ipc.{ MultiClientServer => IpcServer }
import sbt.protocol.{ Envelope, Request, ConfirmRequest, ConfirmResponse, ReadLineRequest, ReadLineResponse, ErrorResponse }
import play.api.libs.json.Format
import sbt.server.ServerRequest
import sbt.server.ExecutionId
import concurrent.{ Promise, promise }
import java.io.EOFException

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

  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  def isAlive: Boolean = clientThread.isAlive && running.get
  private object clientThread extends Thread(s"sbt-client-handler-$configName-$uuid") {
    final override def run(): Unit = {
      while (running.get) {
        try readNextMessage()
        catch {
          case e: EOFException =>
            log.log("Client closed!, shutting down.")
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
      // Here we tell the server thread handler...
      closed()
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

  // ipc is synchronized, so this is ok.
  def send[T: Format](msg: T): Unit = {
    // For now we start ignoring the routing...
    log.log(s"Sending msg to client $configName-$uuid: $msg")
    if (isAlive) ipc.replyJson(0L, msg)
  }
  // ipc is synchronized, so this is ok.
  def reply[T: Format](serial: Long, msg: T): Unit = {
    // For now we start ignoring the routing...
    log.log(s"Sending reply to client $configName-$uuid: $msg")
    if (isAlive) ipc.replyJson(serial, msg)
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
        val result = promise[Option[String]]
        val newSerial = ipc.sendJson(ReadLineRequest(executionId.id, prompt, mask))
        readLineRequests += newSerial -> result
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
        val result = promise[Boolean]
        val newSerial = ipc.sendJson(ConfirmRequest(executionId.id, msg))
        confirmRequests += newSerial -> result
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
  }
  def join(): Unit = clientThread.join()

  override def equals(o: Any): Boolean =
    o match {
      case x: SbtClientHandler => uuid == x.uuid
      case _ => false
    }
  override def hashCode = uuid.hashCode
  override def toString = "LiveClient(" + configName + " " + uuid + ")"
}