package com.typesafe.sbtrc
package server

import ipc.{Server=>IpcServer, JsonWriter}
import com.typesafe.sbtrc.protocol.{Envelope, Request}
import ipc.{Server => IpcServer}


case class ClientRequest(client: SbtClient, serial: Long, msg: Request)
/** An interface we can use to send messages to an sbt client. */
sealed trait SbtClient {
  def send[T: JsonWriter](msg: T): Unit
  /** Creates a new client that will send events to *both* of these clients. */
  def zip(other: SbtClient): SbtClient = (this, other) match {
    case (JoinedSbtClient(clients), JoinedSbtClient(clients2)) => JoinedSbtClient(clients ++ clients2)
    case (JoinedSbtClient(clients), other) => JoinedSbtClient(clients + other)
    case (other, JoinedSbtClient(clients2)) => JoinedSbtClient(clients2 + other)
    case (other, other2) => JoinedSbtClient(Set(other, other2))
  }
  def without(client: SbtClient): SbtClient = 
    this match {
      case `client` | NullSbtClient => NullSbtClient
      case JoinedSbtClient(clients) if clients.contains(client) =>
        JoinedSbtClient(clients filterNot (_ == client))
      case other => other
    }
}
object NullSbtClient extends SbtClient {
  def send[T: JsonWriter](msg: T): Unit = ()
  override def toString = "NullSbtClient"
}
case class JoinedSbtClient(clients: Set[SbtClient]) extends SbtClient {
  final def send[T: JsonWriter](msg: T): Unit = 
    clients foreach (_ send msg)
  override def toString = clients.mkString("Joined(",",",")")
}

/** This class represents an external client into the sbt server.
 *
 * We forward messages from the client into the sbt build loop.
 */
class SbtClientHandler (
    val id: String, 
    ipc: IpcServer,
    msgHandler: ClientRequest => Unit,
    closed: () => Unit) extends SbtClient {
  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  def isAlive: Boolean = clientThread.isAlive && running.get
  private object clientThread extends Thread {
    final override def run(): Unit = {
      while(running.get) {
        try readNextMessage()
        catch {
          case e: Throwable =>
            // On any throwable, we'll shut down this connection as bad.
            running.set(false)
        }
      }
      // Send the stopped message to this client
      try send(protocol.Stopped)
      catch {
        case e: Exception =>
          // We ignore any exception trying to stop things.
      }
      // Here we send a client disconnected message to the main sbt
      // engine so it stops using this client.
      msgHandler(ClientRequest(SbtClientHandler.this, 0L, protocol.ClientClosedRequest()))
      // Here we tell the server thread handler...
      closed()
    }
    private def readNextMessage(): Unit = {
      Envelope(ipc.receive()) match {
          case Envelope(serial, _, msg: Request) =>
            val request = ClientRequest(SbtClientHandler.this, serial, msg)
            msgHandler(request)
          case Envelope(_,_,msg) =>
            sys.error("Unable to handle client request: " + msg)
        }
    }
  }
  // Automatically start listening for client events.
  clientThread.start()
  
  // ipc is synchronized, so this is ok.
  def send[T: JsonWriter](msg: T): Unit = {
    // For now we start ignoring the routing...
    if(isAlive) ipc.replyJson(0L, msg)
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
  override def toString = "LiveClient("+id+")"
}