package com.typesafe.sbtrc
package server

import ipc.{ MultiClientServer => IpcServer }
import sbt.protocol.{ Envelope, Request }
import play.api.libs.json.Format
import sbt.server.ServerRequest

/**
 * This class represents an external client into the sbt server.
 *
 * We forward messages from the client into the sbt build loop.
 */
class SbtClientHandler(
  val id: String,
  ipc: IpcServer,
  msgHandler: ServerRequest => Unit,
  closed: () => Unit) extends sbt.server.LiveClient {

  // TODO - Configure this location.
  // TODO - Is this thread safe-ish?
  private val log = new FileLogger(new java.io.File(s".sbtserver/connections/${id}.log"))

  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  def isAlive: Boolean = clientThread.isAlive && running.get
  private object clientThread extends Thread(s"sbt-client-handler-$id") {
    final override def run(): Unit = {
      while (running.get) {
        try readNextMessage()
        catch {
          case e: Throwable =>
            // On any throwable, we'll shut down this connection as bad.
            log.error(s"Client $id had error, shutting down", e)
            e.printStackTrace(System.err)
            running.set(false)
        }
      }
      log.log(s"Stopping client.")
      // Send the stopped message to this client
      try send(sbt.protocol.Stopped)
      catch {
        case e: Exception =>
          // We ignore any exception trying to stop things.
          log.log(s"Error trying to stop this client: ${e.getMessage}")
      }
      // It's ok to close this connection when we're done.
      ipc.close()
      // Here we send a client disconnected message to the main sbt
      // engine so it stops using this client.
      msgHandler(ServerRequest(SbtClientHandler.this, 0L, sbt.protocol.ClientClosedRequest()))
      // Here we tell the server thread handler...
      closed()
    }
    private def readNextMessage(): Unit = {
      log.log("Reading next message from client.")
      Envelope(ipc.receive()) match {
        case Envelope(serial, _, msg: Request) =>
          log.log(s"Got request: $msg")
          reply(serial, sbt.protocol.ReceivedResponse())
          val request = ServerRequest(SbtClientHandler.this, serial, msg)
          msgHandler(request)
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
    log.log(s"Sending msg to client $id: $msg")
    if (isAlive) ipc.replyJson(0L, msg)
  }
  // ipc is synchronized, so this is ok.
  def reply[T: Format](serial: Long, msg: T): Unit = {
    // For now we start ignoring the routing...
    log.log(s"Sending reply to client $id: $msg")
    if (isAlive) ipc.replyJson(serial, msg)
  }
  def readLine(prompt: String, mask: Boolean): concurrent.Future[Option[String]] = {
    val result = concurrent.promise[Option[String]]
    // TODO - Fix this.
    result.failure(new Error("Not implemented"))
    result.future
  }
  def confirm(msg: String): concurrent.Future[Boolean] = {
    val result = concurrent.promise[Boolean]
    // TODO - Fix this.
    result.failure(new Error("Not implemented"))
    result.future
  }

  def shutdown(): Unit = {
    running.set(false)
  }
  def join(): Unit = clientThread.join()

  override def equals(o: Any): Boolean =
    o match {
      case x: SbtClientHandler => id == x.id
      case _ => false
    }
  override def hashCode = id.hashCode
  override def toString = "LiveClient(" + id + ")"
}