package com.typesafe.sbtrc
package client

import sbt.client._
import java.io.File
import scala.concurrent._
import sbt.protocol._
import scala.util.control.NonFatal
import scala.util.{ Try, Success, Failure }
import java.io.Closeable

// This thread is responsible for ONE attempt to connect, then we drop
// the thread (don't want to keep it around when it's typically needed
// only briefly).
// Important: closeHandler guaranteed to be called only AFTER doneHandler
private final class ConnectThread(doneHandler: Try[SbtClient] => Unit,
  closeHandler: () => Unit,
  sleepMilliseconds: Long, configName: String, humanReadableName: String,
  directory: File, locator: SbtServerLocator) extends Thread with Closeable {
  import scala.concurrent.ExecutionContext.Implicits.global
  val sleepRemaining = new java.util.concurrent.atomic.AtomicLong(sleepMilliseconds)
  @volatile var closed = false

  // This is a little trick to ensure that the closeHandler
  // is always called only AFTER the doneHandler
  val donePromise = Promise[SbtClient]()
  val closedPromise = Promise[Unit]()
  donePromise.future.onComplete(doneHandler)
  // BOTH promises are needed to run the closeHandler
  Future.sequence(Seq(donePromise.future, closedPromise.future)).onComplete(whatever => closeHandler())

  private def adjustRemaining(block: Long => Long): Unit = {
    while (!{
      val remaining = sleepRemaining.get
      sleepRemaining.compareAndSet(remaining, block(remaining))
    }) {
      // do nuthin'
    }
  }

  override def run(): Unit = try {
    while (sleepRemaining.get > 0) {
      Thread.sleep(Math.min(200, sleepRemaining.get))
      // if this puts us below 0 it's fine
      adjustRemaining(_ - 200)
    }
    if (closed)
      throw new RuntimeException("Not reconnecting because SbtConnector was closed")

    donePromise.success(connectToSbt())
  } catch {
    case NonFatal(e) =>
      donePromise.failure(e)
  }

  def connectInNoMoreThan(ms: Long): Unit = {
    adjustRemaining(r => Math.min(r, ms))
  }

  override def close(): Unit = {
    closed = true
  }

  private[this] def connectToSbt(): SbtClient = {
    val uri = locator.locate(directory)
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
    new SimpleSbtClient(uuid, configName, humanReadableName, rawClient, () => closedPromise.success(()))
  }
}

class SimpleConnector(configName: String, humanReadableName: String, directory: File, locator: SbtServerLocator) extends SbtConnector {
  private var currentClient: Option[SbtClient] = None
  // this should only exist if currentClient.isEmpty since its job
  // is to try to make a new client
  private var currentConnectThread: Option[ConnectThread] = None
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

  private def startConnectAttempt(afterMilliseconds: Long): Unit = {
    require(currentClient.isEmpty)
    require(currentConnectThread.isEmpty)
    currentConnectThread = Some(new ConnectThread(onConnectionAttempt, onClose,
      afterMilliseconds,
      configName, humanReadableName, directory, locator))
    currentConnectThread.foreach(_.start())
  }

  def open(): Unit = synchronized {
    if (currentClient.isEmpty) {
      currentConnectThread match {
        case Some(thread) =>
          // force immediate retry
          thread.connectInNoMoreThan(0)
        case None =>
          startConnectAttempt(0 /* immediate attempt */ )
      }
    }
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
      case None =>
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

  private def notifyAndRecoverFromError(message: String): Unit = synchronized {
    for (listener <- errorListeners)
      listener.emit(reconnecting, message)

    if (reconnecting) {
      currentConnectThread match {
        case Some(thread) => // already retrying
        case None =>
          startConnectAttempt(4000 /* retry every this many milliseconds */ )
      }
    } else {
      // It's impossible for these to be called again
      // because once reconnecting=false it's always false,
      // AND we just notified on the final fatal error.
      // So clear them out to allow GC - these may well capture a big
      // hunk of application functionality.
      connectListeners = Nil
      errorListeners = Nil
    }
  }

  // A callback from our connecting thread when it's done; always called before
  // onClose
  private def onConnectionAttempt(result: Try[SbtClient]): Unit = synchronized {
    require(currentClient.isEmpty) // we shouldn't have created a thread if we had a client
    currentConnectThread.foreach(_.join())
    currentConnectThread = None
    result match {
      case Failure(error) =>
        notifyAndRecoverFromError(error.getMessage)
      case Success(client) =>
        currentClient = Some(client)
        for (listener <- connectListeners)
          listener.emit(client)
    }
  }

  // A callback from the server handling thread.
  private def onClose(): Unit = synchronized {
    // we shouldn't have received a close callback without first getting
    // an onConnectionAttempt with a client
    require(currentClient.isDefined)
    notifyAndRecoverFromError("Connection closed")
  }

  def close(): Unit = synchronized {
    reconnecting = false
    // don't set these to None here - it should happen
    // in the appropriate callbacks
    currentConnectThread.foreach(_.close())
    currentClient.foreach(_.close())
  }
}
