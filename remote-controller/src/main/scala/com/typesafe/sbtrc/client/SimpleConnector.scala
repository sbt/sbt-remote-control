package com.typesafe.sbtrc
package client

import com.typesafe.sbtrc.api._
import java.io.File
import scala.concurrent.ExecutionContext

class SimpleConnector(directory: File, locator: SbtServerLocator) extends SbtConnector {
  @volatile private var currentClient: Option[SbtClient] = None
  @volatile private var listeners: List[Listener] = Nil
  @volatile private var reconnecting: Boolean = true

  // Helper to give listeners an identity and execute methods on the given
  // context.
  class Listener(handler: SbtClient => Unit, ctx: ExecutionContext) {
    private val id = java.util.UUID.randomUUID.toString
    def emit(client: SbtClient): Unit =
      ctx.prepare.execute(new Runnable() {
        override def run(): Unit = {
          handler(client)
        }
      })
    override def hashCode = id.hashCode
    override def equals(o: Any) =
      o match {
        case other: Listener => id == other.id
        case _ => false
      }
  }

  def onConnect(handler: SbtClient => Unit)(implicit ex: ExecutionContext): Subscription = {
    val listener = new Listener(handler, ex)
    listeners = listener :: listeners
    object sub extends Subscription {
      def cancel(): Unit = {
        listeners = listeners.filterNot(_ == listener)
      }
    }
    handleNewSubscriber(listener)
    sub
  }
  private def handleNewSubscriber(listener: Listener): Unit = synchronized {
    currentClient match {
      case Some(client) => listener emit client
      case None => connectToSbt()
    }
  }

  private def connectToSbt(): Unit = synchronized {
    val uri = locator.locate(directory)
    // TODO - We need  way to be notified of failures so we can reconnect here...
    val socket = new java.net.Socket(uri.getHost, uri.getPort)
    val rawClient = new ipc.Client(socket)
    val sbtClient = new SimpleSbtClient(rawClient, onClose)
    currentClient = Some(sbtClient)
    // notify all existing folks of the new client.
    def loop(remaining: List[Listener]): Unit =
      remaining match {
        case Nil => ()
        case head :: tail =>
          head emit sbtClient
          loop(tail)
      }
    loop(listeners)
  }
  // A callback from the server handling thread.
  private def onClose(): Unit = synchronized {
    if (reconnecting) connectToSbt()
  }

  def close(): Unit = synchronized {
    reconnecting = false
    currentClient foreach (_.close())
    currentClient = None
    // TODO - Is this the right way to go?
    listeners = Nil
  }
}