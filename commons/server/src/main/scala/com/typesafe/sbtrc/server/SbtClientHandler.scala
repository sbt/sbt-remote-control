package com.typesafe.sbtrc
package server

import ipc.{Server=>IpcServer, JsonWriter}
import com.typesafe.sbtrc.protocol.{Envelope, Request}
import ipc.{Server => IpcServer}


case class ClientRequest(client: SbtClient, serial: Long, msg: Request)
/** An interface we can use to send messages to an sbt client. */
trait SbtClient {
  def send[T: JsonWriter](msg: T): Unit
  /** Creates a new client that will send events to *both* of these clients. */
  def zip(other: SbtClient): SbtClient = (this, other) match {
    case (JoinedSbtClient(clients), JoinedSbtClient(clients2)) => JoinedSbtClient(clients ++ clients2)
    case (JoinedSbtClient(clients), other) => JoinedSbtClient(other :: clients)
    case (other, JoinedSbtClient(clients2)) => JoinedSbtClient(other :: clients2)
    case (other, other2) => JoinedSbtClient(other :: other2 :: Nil)
  }
}
object NullSbtClient extends SbtClient {
  def send[T: JsonWriter](msg: T): Unit = ()
  override def toString = "NullSbtClient"
}
case class JoinedSbtClient(clients: List[SbtClient]) extends SbtClient {
  final def send[T: JsonWriter](msg: T): Unit = 
    clients foreach (_ send msg)
  override def toString = clients.mkString("Joined(",",",")")
}

/** This class represents an external client into the sbt server.
 *
 * We forward messages from the client into the sbt build loop.
 */
class SbtClientHandler (
    id: String, 
    ipc: IpcServer,
    msgHandler: ClientRequest => Unit) extends SbtClient {
  private val running = new java.util.concurrent.atomic.AtomicBoolean(true)
  def isAlive: Boolean = thread.isAlive && running.get
  private val thread = new Thread {
    final override def run(): Unit = {
      while(running.get) {
        Envelope(ipc.receive()) match {
          case Envelope(serial, _, msg: Request) =>
            val request = ClientRequest(SbtClientHandler.this, serial, msg)
            println("Received request: " + request)
            msgHandler(request)
          case Envelope(_,_,msg) =>
            sys.error("Unable to handle client request: " + msg)
        }
      }
      // Send the stopped message to this client
      send(protocol.Stopped)
    }
  }
  // Automatically start listening for client events.
  thread.start()
  
  // ipc is synchronized, so this is ok.
  def send[T: JsonWriter](msg: T): Unit = {
    // For now we start ignoring the routing...
    if(isAlive) ipc.replyJson(0L, msg)
  }

  def shutdown(): Unit = {
    running.set(false)
  }
  def join(): Unit = thread.join()
  
  override def toString = "LiveClient("+id+")"
}