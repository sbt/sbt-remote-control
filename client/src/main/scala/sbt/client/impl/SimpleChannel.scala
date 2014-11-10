
package sbt.client.impl

import sbt.impl.ipc
import sbt.protocol
import sbt.protocol._
import sbt.serialization._
import sbt.client.{ SbtChannel, Subscription }
import scala.concurrent.{ ExecutionContext, Future, Promise }
import java.net.SocketException
import scala.util.control.NonFatal
import java.io.IOException
import java.io.EOFException
import java.io.Closeable

/**
 * A concrete implementation of the SbtChannel trait.
 */
final private class SimpleSbtChannel(override val uuid: java.util.UUID,
  override val configName: String,
  override val humanReadableName: String,
  socket: ipc.Client, closeHandler: () => Unit) extends SbtChannel {

  // counted down when we should start sending events
  private val claimed = new java.util.concurrent.atomic.AtomicBoolean(false)
  private val claimedLatch = new java.util.concurrent.CountDownLatch(1)

  // We have two things we want to do with the sendJson error:
  // either report it in the Future if we are going to return a Future,
  // or ignore it. We never want to report it synchronously
  // because that makes it super annoying for users of SbtClient to
  // deal with closed clients.

  // sendJson and wrap the failure or serial in a Future
  // (this is actually a synchronous operation but we want to
  // make it look async so we don't synchronously throw)
  private def sendJson[T <: Message](message: T, serial: Long): Future[Unit] = {
    try Future.successful(socket.sendJson[Message](message, serial))
    catch {
      case NonFatal(e) =>
        Future.failed(e)
    }
  }

  override def sendJson[T <: Message](message: T): Future[Unit] =
    sendJson(message, socket.serialGetAndIncrement())

  // sendJson, providing a registration function which provides a future
  // representing the reply. The registration function would complete its
  // future by finding a reply with the serial passed to the registration
  // function.
  override def sendJsonWithRegistration[T <: Message, R](message: T)(registration: Long => Future[R]): Future[R] = {
    import concurrent.ExecutionContext.Implicits.global
    val serial = socket.serialGetAndIncrement()
    val result = registration(serial)
    // TODO we should probably arrange for this to time out and to get an error
    // on client close, right now the future can remain incomplete indefinitely
    // unless the registration function avoids that (some of ours do though by
    // completing the future when the client is closed).
    sendJson(message, serial) flatMap { _ => result }
  }

  override def replyJson[T <: Message](replyTo: Long, message: T): Future[Unit] =
    try Future.successful(socket.replyJson[Message](replyTo, message))
    catch {
      case NonFatal(e) => Future.failed(e)
    }

  private val messageListeners = new ListenerSet()

  override def handleMessages(listener: Envelope => Unit)(implicit ex: ExecutionContext): Subscription =
    messageListeners.add(listener)(ex)

  override def claimMessages(listener: Envelope => Unit)(implicit ex: ExecutionContext): Subscription = {
    if (claimed.getAndSet(true)) {
      throw new sbt.client.ChannelInUseException()
    } else {
      // we have to handle messages before we release the
      // latch so we don't miss any messages
      val sub = handleMessages(listener)
      // tell the message thread to start handling messages
      claimedLatch.countDown()
      sub
    }
  }

  @volatile var running = true

  override def close(): Unit = {
    running = false
    // Here we force the client to close so it interrupts the read thread and we can kill the process, otherwise we may
    // never stop in any reasonable time.
    socket.close()
    // in case we are waiting on the claimed latch,
    // also interrupt
    thread.interrupt()
    // other cleanup all happens on the thread
    thread.join()
  }

  object thread extends Thread {
    override def run(): Unit = {
      // wait until someone claims events before we start to read them
      try claimedLatch.await()
      catch {
        case e: InterruptedException =>
          // this happens if we close() before anyone
          // ever does a claimMessages()
          running = false
      }

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
          case e: InterruptedException =>
            // if this was from close() then running should be false already,
            // if it was from someone else it's not clear what happened
            // but I guess it would be safest to stop running.
            running = false
        }
      }
      // Here we think sbt connection has closed.

      // make sure it's marked as closed on client side
      socket.close()

      // this synthesizes ClosedEvent to all listeners
      messageListeners.close(lastReceivedSerial + 1)
      // this is so our spawning connector doesn't need to listen to all
      // messages just to get ClosedEvent
      closeHandler()
    }
  }

  thread.start()

  override def isClosed: Boolean = socket.isClosed
}

/** Store a bunch of event listeners */
private final class ListenerSet {

  /** A wrapped event listener that ensures events are fired on the desired execution context. */
  private final class Listener(listener: Envelope => Unit, ex: ExecutionContext) {
    private val id = java.util.UUID.randomUUID
    def send(e: Envelope): Unit = {
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

  def add(listener: Envelope => Unit)(implicit ex: ExecutionContext): Subscription = {
    val wrapped = new Listener(listener, ex)
    if (closeEventSerial == 0L)
      listeners += wrapped
    else
      wrapped.send(protocol.Envelope(serial = closeEventSerial, replyTo = 0L, ClosedEvent()))
    object subscription extends Subscription {
      def cancel(): Unit =
        listeners -= wrapped
    }
    subscription
  }

  private var listeners = Set.empty[Listener]
  // if nonzero, we are closed
  private var closeEventSerial = 0L

  def send(e: Envelope): Unit = synchronized {
    listeners foreach { l =>
      try l send e
      catch {
        case NonFatal(_) => // Ignore non fatal exceptions while sending events.
      }
    }
  }

  def close(closeEventSerial: Long): Unit = synchronized {
    this.closeEventSerial = closeEventSerial
    // ClosedEvent is our only client-side-synthesized thing,
    // because there's no way for anyone to know otherwise.
    send(protocol.Envelope(serial = closeEventSerial, replyTo = 0L, ClosedEvent()))
    listeners = Set.empty
  }
}

