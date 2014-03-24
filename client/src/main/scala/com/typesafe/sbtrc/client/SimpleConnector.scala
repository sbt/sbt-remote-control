package com.typesafe.sbtrc
package client

import sbt.client._
import java.io.File
import scala.concurrent.ExecutionContext
import sbt.protocol._

class SimpleConnector(configName: String, humanReadableName: String, directory: File, locator: SbtServerLocator) extends SbtConnector {
  private var currentClient: Option[SbtClient] = None
  private var connectListeners: List[ConnectListener] = Nil
  private var reconnecting: Boolean = true

  private final class ConnectListener(handler: SbtClient => Unit, ctx: ExecutionContext) {
    def emit(client: SbtClient): Unit =
      ctx.prepare.execute(new Runnable() {
        override def run(): Unit = {
          handler(client)
        }
      })
  }

  def onConnect(handler: SbtClient => Unit)(implicit ex: ExecutionContext): Subscription = {
    val listener = new ConnectListener(handler, ex)
    SimpleConnector.this.synchronized(connectListeners = listener :: connectListeners)
    object sub extends Subscription {
      def cancel(): Unit = {
        SimpleConnector.this.synchronized(connectListeners = connectListeners.filterNot(_ == listener))
      }
    }
    handleNewSubscriber(listener)
    sub
  }
  private[this] def handleNewSubscriber(listener: ConnectListener): Unit = synchronized {
    currentClient match {
      case Some(client) => listener emit client
      case None => connectToSbt()
    }
  }

  private[this] def connectToSbt(): Unit = synchronized {
    val uri = locator.locate(directory)
    // TODO - We need  way to be notified of failures so we can reconnect here...
    val socket = new java.net.Socket(uri.getHost, uri.getPort)
    val rawClient = new ipc.Client(socket)
    val uuid = java.util.UUID.randomUUID()
    val registerSerial = rawClient.sendJson(RegisterClientRequest(uuid.toString, configName, humanReadableName))
    Envelope(rawClient.receive()) match {
      case Envelope(_, `registerSerial`, ErrorResponse(message)) =>
        throw new RuntimeException(s"Failed to register client with sbt: ${message}")
      case Envelope(_, `registerSerial`, reply: ReceivedResponse) =>
      case wtf => {
        rawClient.close()
        throw new RuntimeException(s"unexpected initial message from server was not a register client reply: ${wtf}")
      }
    }
    val sbtClient = new SimpleSbtClient(uuid, configName, humanReadableName, rawClient, () => onClose())
    currentClient = Some(sbtClient)
    // notify all existing folks of the new client.
    def loop(remaining: List[ConnectListener]): Unit =
      remaining match {
        case Nil => ()
        case head :: tail =>
          head emit sbtClient
          loop(tail)
      }
    loop(connectListeners)
  }
  // A callback from the server handling thread.
  private def onClose(): Unit = synchronized {
    // TODO - This should only happen if we think the connection to the
    // sbt server is closed.
    if (reconnecting) connectToSbt()
  }

  def close(): Unit = {
    synchronized { reconnecting = false }
    currentClient foreach (_.close())
    synchronized {
      currentClient = None
      // TODO - Is this the right way to go?
      connectListeners = Nil
    }
  }
}