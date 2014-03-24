package com.typesafe.sbtrc
package client

import sbt.client._
import java.io.File
import scala.concurrent.ExecutionContext
import sbt.protocol._
import scala.util.control.NonFatal

class SimpleConnector(configName: String, humanReadableName: String, directory: File, locator: SbtServerLocator) extends SbtConnector {
  private var currentClient: Option[SbtClient] = None
  private var connectListeners: List[ConnectListener] = Nil
  private var errorListeners: List[ErrorListener] = Nil
  private var reconnecting: Boolean = true

  private final class ConnectListener(handler: SbtClient => Unit, ctx: ExecutionContext) {
    def emit(client: SbtClient): Unit =
      ctx.prepare.execute(new Runnable() {
        override def run(): Unit = {
          handler(client)
        }
      })
  }

  private final class ErrorListener(handler: (Boolean, String) => Unit, ctx: ExecutionContext) {
    def emit(reconnecting: Boolean, message: String): Unit =
      ctx.prepare.execute(new Runnable() {
        override def run(): Unit = {
          handler(reconnecting, message)
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
    handleNewConnectSubscriber(listener)
    sub
  }
  private[this] def handleNewConnectSubscriber(listener: ConnectListener): Unit = synchronized {
    currentClient match {
      case Some(client) => listener emit client
      case None => connectToSbt()
    }
  }
  def onError(handler: (Boolean, String) => Unit)(implicit ex: ExecutionContext): Subscription = {
    val listener = new ErrorListener(handler, ex)
    SimpleConnector.this.synchronized(errorListeners = listener :: errorListeners)
    object sub extends Subscription {
      def cancel(): Unit = {
        SimpleConnector.this.synchronized(errorListeners = errorListeners.filterNot(_ == listener))
      }
    }
    sub
  }

  // TODO the error handling here makes little or no sense. We have to make
  // connection async instead of a side effect of onConnect, to fix it.
  private[this] def connectToSbt(): Unit = try synchronized {
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
  } catch {
    case NonFatal(e) =>
      // somewhat oddly, reconnecting can change midstream
      // if one of the error listeners calls close()
      for (listener <- errorListeners)
        listener.emit(reconnecting, e.getMessage)
      throw e
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