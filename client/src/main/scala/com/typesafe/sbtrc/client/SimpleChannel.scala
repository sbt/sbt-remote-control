package com.typesafe.sbtrc
package client

import sbt.protocol
import sbt.protocol._
import sbt.client.{ SbtChannel, Subscription }
import scala.concurrent.{ ExecutionContext, Future, Promise }
import java.net.SocketException
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.control.NonFatal
import java.io.IOException
import java.io.EOFException
import java.io.Closeable
import play.api.libs.json.Writes

/**
 * A concrete implementation of the SbtChannel trait.
 */
final private class SimpleSbtChannel(override val uuid: java.util.UUID,
  override val configName: String,
  override val humanReadableName: String,
  socket: ipc.Client, closeHandler: () => Unit) extends SbtChannel {

  private var claimed = false

  /**
   * Called once by whoever will use the channel; if called twice it throws ChannelInUseException.
   *  This is just to provide fail-fast if you try to wrap the same channel in multiple clients or something.
   */
  override def claim(): Unit = synchronized {
    if (claimed)
      throw new sbt.client.ChannelInUseException()
    claimed = true
  }

  // We have two things we want to do with the sendJson error:
  // either report it in the Future if we are going to return a Future,
  // or ignore it. We never want to report it synchronously
  // because that makes it super annoying for users of SbtClient to
  // deal with closed clients.

  // sendJson and wrap the failure or serial in a Future
  // (this is actually a synchronous operation but we want to
  // make it look async so we don't synchronously throw)
  private def sendJson[T: Writes](message: T, serial: Long): Future[Unit] = {
    try Future.successful(socket.sendJson(message, serial))
    catch {
      case NonFatal(e) =>
        Future.failed(e)
    }
  }

  override def sendJson[T: Writes](message: T): Future[Unit] =
    sendJson(message, socket.serialGetAndIncrement())

  // sendJson, providing a registration function which provides a future
  // representing the reply. The registration function would complete its
  // future by finding a reply with the serial passed to the registration
  // function.
  override def sendJsonWithRegistration[T: Writes, R](message: T)(registration: Long => Future[R]): Future[R] = {
    import concurrent.ExecutionContext.Implicits.global
    val serial = socket.serialGetAndIncrement()
    val result = registration(serial)
    // TODO we should probably arrange for this to time out and to get an error
    // on client close, right now the future can remain incomplete indefinitely
    // unless the registration function avoids that (some of ours do though by
    // completing the future when the client is closed).
    sendJson(message, serial) flatMap { _ => result }
  }

  override def replyJson[T: Writes](replyTo: Long, message: T): Future[Unit] =
    try Future.successful(socket.replyJson(replyTo, message))
    catch {
      case NonFatal(e) => Future.failed(e)
    }

  private val messageListeners = new ListenerSet[Envelope]()

  override def handleMessages(listener: Envelope => Unit)(implicit ex: ExecutionContext): Subscription =
    messageListeners.add(listener)(ex)

  @volatile var running = true

  override def close(): Unit = {
    running = false
    // Here we force the client to close so it interrupts the read thread and we can kill the process, otherwise we may
    // never stop in any reasonable time.
    socket.close()
    // other cleanup all happens on the thread
    thread.join()
  }

  object thread extends Thread {
    override def run(): Unit = {
      // we save our serials so we can synthesize ClosedEvent
      var lastReceivedSerial = 0L
      while (running) {
        try {
          val envelope = protocol.Envelope(socket.receive())
          lastReceivedSerial = envelope.serial
          messageListeners.send(envelope)
        } catch {
          case e @ (_: SocketException | _: EOFException | _: IOException) =>
            // don't print anything here, this is a normal occurrence when server
            // restarts or the socket otherwise closes for some reason.
            // Closing the socket can cause "IOException: Stream closed"
            // when reading a stream or in other cases we might get a socket
            // exception or EOFException perhaps.
            running = false
        }
      }
      // Here we think sbt connection has closed.

      // make sure it's marked as closed on client side
      socket.close()

      // ClosedEvent is our only client-side-synthesized thing,
      // because there's no way for anyone to know otherwise.
      messageListeners.send(protocol.Envelope(serial = lastReceivedSerial + 1, replyTo = 0L, ClosedEvent()))
      messageListeners.close()
      // this is so our spawning connector doesn't need to listen to all
      // messages just to get ClosedEvent
      closeHandler()
    }
  }

  thread.start()

  override def isClosed: Boolean = socket.isClosed
}

/** Store a bunch of event listeners */
private final class ListenerSet[Event] extends Closeable {

  /** A wrapped event listener that ensures events are fired on the desired execution context. */
  private final class Listener(listener: Event => Unit, ex: ExecutionContext) {
    private val id = java.util.UUID.randomUUID
    def send(e: Event): Unit = {
      // TODO - do we need to prepare the context?
      ex.prepare.execute(new Runnable() {
        def run(): Unit = {
          listener(e)
        }
      })
    }

    override def hashCode = id.hashCode
    override def equals(o: Any): Boolean = o match {
      case x: Listener => x.id == id
      case _ => false
    }
  }

  def add(listener: Event => Unit)(implicit ex: ExecutionContext): Subscription = {
    val wrapped = new Listener(listener, ex)
    addEventListener(wrapped)
    object subscription extends Subscription {
      def cancel(): Unit =
        removeEventListener(wrapped)
    }
    subscription
  }

  private var listeners = Set.empty[Listener]

  private def addEventListener(l: Listener): Unit = synchronized {
    listeners += l
  }
  private def removeEventListener(l: Listener): Unit = synchronized {
    listeners -= l
  }
  def send(e: Event): Unit = synchronized {
    listeners foreach { l =>
      try l send e
      catch {
        case NonFatal(_) => // Ignore non fatal exceptions while sending events.
      }
    }
  }

  override def close(): Unit = synchronized {
    // Don't send events to these listeners anymore.
    while (listeners.nonEmpty)
      removeEventListener(listeners.head)
  }
}

