package com.typesafe.sbtrc
package client

import sbt.protocol
import sbt.protocol._
import sbt.client.{ SbtClient, Subscription, BuildStructureListener, EventListener, ValueListener, SettingKey, TaskKey, Interaction }
import scala.concurrent.{ ExecutionContext, Future, Promise }
import java.net.SocketException
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.control.NonFatal
import java.io.IOException
import java.io.EOFException
import java.io.Closeable
import play.api.libs.json.Format

/**
 * Very terrible implementation of the sbt client.
 *
 *
 * Not only is it blockin', we have mutability hell and lockin'.
 *
 * This is only to do proof of concept work and flesh out the server.
 */
class SimpleSbtClient(override val uuid: java.util.UUID,
  override val configName: String,
  override val humanReadableName: String,
  client: ipc.Client, closeHandler: () => Unit) extends SbtClient {

  import scala.concurrent.ExecutionContext.Implicits.global

  // We have two things we want to do with the sendJson error:
  // either report it in the Future if we are going to return a Future,
  // or ignore it. We never want to report it synchronously
  // because that makes it super annoying for users of SbtClient to
  // deal with closed clients.

  // sendJson and wrap the failure or serial in a Future
  // (this is actually a synchronous operation but we want to
  // make it look async so we don't synchronously throw)
  private def sendJson[T: Format](message: T, serial: Long): Future[Unit] = {
    try Future.successful(client.sendJson(message, serial))
    catch {
      case NonFatal(e) =>
        Future.failed(e)
    }
  }

  private def sendJson[T: Format](message: T): Future[Unit] =
    sendJson(message, client.serialGetAndIncrement())

  // sendJson, providing a registration function which provides a future
  // representing the reply. The registration function would complete its
  // future by finding a reply with the serial passed to the registration
  // function.
  private def sendJsonWithRegistration[T: Format, R](message: T)(registration: Long => Future[R]): Future[R] = {
    val serial = client.serialGetAndIncrement()
    val result = registration(serial)
    // TODO we should probably arrange for this to time out and to get an error
    // on client close, right now the future can remain incomplete indefinitely
    // unless the registration function avoids that (some of ours do though by
    // completing the future when the client is closed).
    sendJson(message, serial) flatMap { _ => result }
  }

  // like sendJsonWithRegistration but provides a prebuilt promise
  private def sendJsonWithResult[T: Format, R](message: T)(registration: (Long, Promise[R]) => Unit): Future[R] = {
    sendJsonWithRegistration(message) { serial =>
      val result = Promise[R]()
      registration(serial, result)
      result.future
    }
  }

  // For now, use the global object.
  override def dynamicSerialization: DynamicSerialization = DynamicSerializaton

  def watchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription = {
    val sub = buildEventManager.watch(listener)(ex)
    // TODO this is really busted; we need to have a local cache of the latest build and provide
    // that local cache ONLY to the new listener, rather than reloading remotely and sending
    // it to ALL existing listeners.
    sendJson(SendSyntheticBuildChanged())
    sub
  }

  def lazyWatchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription =
    buildEventManager.watch(listener)(ex)

  def possibleAutocompletions(partialCommand: String, detailLevel: Int): Future[Set[Completion]] = {
    sendJsonWithResult(CommandCompletionsRequest(partialCommand, detailLevel)) { (serial: Long, result: Promise[Set[Completion]]) =>
      completionsManager.register(serial, result)
    }
  }

  def lookupScopedKey(name: String): Future[Seq[ScopedKey]] = {
    sendJsonWithResult(KeyLookupRequest(name)) { (serial: Long, result: Promise[Seq[ScopedKey]]) =>
      keyLookupRequestManager.register(serial, result)
    }
  }
  def requestExecution(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] = {
    sendJsonWithRegistration(ExecutionRequest(commandOrTask)) { serial =>
      requestHandler.register(serial, interaction).received
    }
  }
  def requestExecution(key: ScopedKey, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] = {
    sendJsonWithRegistration(KeyExecutionRequest(key)) { serial =>
      requestHandler.register(serial, interaction).received
    }
  }
  def cancelExecution(id: Long): Future[Boolean] =
    sendJsonWithRegistration(CancelExecutionRequest(id)) { serial =>
      cancelRequestManager.register(serial)
    }

  def handleEvents(listener: EventListener)(implicit ex: ExecutionContext): Subscription =
    eventManager.watch(listener)(ex)

  def watch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription = {
    val sub = lazyWatch(key)(listener)
    // TODO this is really busted; we need to have a local cache of the latest value and provide
    // that local cache ONLY to the new listener, rather than reloading remotely and sending
    // it to ALL existing listeners.
    sendJson(SendSyntheticValueChanged(key.key))
    sub
  }
  def lazyWatch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription =
    valueEventManager[T](key.key).watch(listener)(ex)

  // TODO - Some mechanisms of listening to interaction here...
  def watch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription = {
    val sub = lazyWatch(key)(listener)
    // TODO this is really busted; we need to have a local cache of the latest value and provide
    // that local cache ONLY to the new listener, rather than reloading remotely and sending
    // it to ALL existing listeners.
    // Right now, anytime we add a listener to a task we re-run that task (!!!)
    // Combined with the issue of adding interaction handlers, having watch() on tasks
    // do a notification right away may simply be a bad idea?
    sendJson(SendSyntheticValueChanged(key.key))
    sub
  }
  def lazyWatch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription =
    valueEventManager[T](key.key).watch(listener)(ex)

  // TODO - Maybe we should try a bit harder here to `kill` the server.
  override def requestSelfDestruct(): Unit =
    sendJson(KillServerRequest())

  // TODO - Implement
  def close(): Unit = {
    running = false
    // Here we force the client to close so it interrupts the read thread and we can kill the process, otherwise we may
    // never stop in any reasonable time.
    client.close()
    thread.join()
  }

  @volatile var running = true
  private object eventManager extends ListenerManager[protocol.Event, EventListener, ListenToEvents, UnlistenToEvents](ListenToEvents(), UnlistenToEvents(), client) {
    override def wrapListener(l: EventListener, ex: ExecutionContext) = new EventListenerHelper(l, ex)
  }
  private object buildEventManager extends ListenerManager[MinimalBuildStructure, BuildStructureListener, ListenToBuildChange, UnlistenToBuildChange](ListenToBuildChange(), UnlistenToBuildChange(), client) {
    override def wrapListener(l: BuildStructureListener, ex: ExecutionContext) = new BuildListenerHelper(l, ex)
  }
  private object valueEventManager extends Closeable {
    private var valueListeners = collection.mutable.Map.empty[ScopedKey, ValueChangeManager[_]]

    def apply[T](key: ScopedKey): ValueChangeManager[T] = synchronized {
      // Yes, we cheat types here...
      valueListeners.get(key) match {
        case Some(mgr) => mgr.asInstanceOf[ValueChangeManager[T]]
        case None =>
          val mgr =
            new ValueChangeManager[Any](key, client).asInstanceOf[ValueChangeManager[T]]
          valueListeners.put(key, mgr)
          mgr
      }
    }
    override def close(): Unit = {
      valueListeners.values.foreach(_.close())
    }
  }
  def completePromisesOnClose(handlers: Map[_, Promise[_]]): Unit = {
    for (promise <- handlers.values)
      promise.failure(new RuntimeException("Connection to sbt closed"))
  }
  private object completionsManager extends Closeable {
    private var handlers: Map[Long, Promise[Set[Completion]]] = Map.empty
    def register(serial: Long, listener: Promise[Set[Completion]]): Unit = synchronized {
      handlers += (serial -> listener)
    }
    def fire(serial: Long, completions: Set[Completion]): Unit = synchronized {
      handlers get serial match {
        case Some(handler) =>
          handler.success(completions)
          handlers -= serial
        case None =>
          System.err.println(s"Received an unexpected reply to serial ${serial}")
      }
    }
    override def close(): Unit = completePromisesOnClose(handlers)
  }

  private object cancelRequestManager extends Closeable {
    private var handlers: Map[Long, Promise[Boolean]] = Map.empty

    def register(id: Long): Future[Boolean] = synchronized {
      val p = Promise[Boolean]()
      handlers += (id -> p)
      p.future
    }
    def fire(id: Long, result: Boolean) = synchronized {
      handlers get id match {
        case Some(promise) =>
          promise.success(result)
          handlers -= id
        case None => //ignore
      }
    }
    def close(): Unit = completePromisesOnClose(handlers)
  }

  private object keyLookupRequestManager extends Closeable {
    private var handlers: Map[Long, Promise[Seq[ScopedKey]]] = Map.empty
    def register(id: Long, listener: Promise[Seq[ScopedKey]]): Unit = synchronized {
      handlers += (id -> listener)
    }
    def fire(id: Long, key: Seq[ScopedKey]): Unit = synchronized {
      handlers get id match {
        case Some(handler) =>
          handler.success(key)
          handlers -= id
        case None => // ignore
      }
    }
    override def close(): Unit = completePromisesOnClose(handlers)
  }

  // TODO - Error handling!
  object thread extends Thread {
    override def run(): Unit = {
      while (running) {
        try handleNextEvent()
        catch {
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
      client.close()

      // shut 'er down.
      eventManager.close() // sends ClosedEvent so do this first
      valueEventManager.close()
      buildEventManager.close()
      requestHandler.close()
      completionsManager.close()
      keyLookupRequestManager.close()
      cancelRequestManager.close()
      closeHandler()
    }
    def handleNextEvent(): Unit =
      protocol.Envelope(client.receive()) match {
        case protocol.Envelope(_, _, e: ValueChanged[_]) =>
          valueEventManager(e.key).sendEvent(e)
        case protocol.Envelope(_, _, e: BuildStructureChanged) =>
          buildEventManager.sendEvent(e.structure)
        case protocol.Envelope(_, replyTo, KeyLookupResponse(key, result)) =>
          keyLookupRequestManager.fire(replyTo, result)
        case protocol.Envelope(_, replyTo, protocol.CommandCompletionsResponse(completions)) =>
          completionsManager.fire(replyTo, completions)
        case protocol.Envelope(_, requestSerial, protocol.ExecutionRequestReceived(executionId)) =>
          requestHandler.executionReceived(requestSerial, executionId)
        case protocol.Envelope(_, _, e: protocol.ExecutionSuccess) =>
          requestHandler.executionDone(e.id)
          eventManager.sendEvent(e)
        case protocol.Envelope(_, replyTo, protocol.CancelExecutionResponse(result)) =>
          cancelRequestManager.fire(replyTo, result)
        case protocol.Envelope(_, _, e: protocol.ExecutionFailure) =>
          requestHandler.executionFailed(e.id, s"execution failed")
          eventManager.sendEvent(e)
        case protocol.Envelope(_, _, e: Event) =>
          eventManager.sendEvent(e)
        case protocol.Envelope(_, requestSerial, protocol.ErrorResponse(msg)) =>
          requestHandler.protocolError(requestSerial, msg)
        case protocol.Envelope(request, _, protocol.ReadLineRequest(executionId, prompt, mask)) =>
          try client.replyJson(request, protocol.ReadLineResponse(requestHandler.readLine(executionId, prompt, mask)))
          catch {
            case NoInteractionException =>
              client.replyJson(request, protocol.ErrorResponse("Unable to handle request: No interaction is defined"))
          }
        case protocol.Envelope(request, _, protocol.ConfirmRequest(executionId, msg)) =>
          try client.replyJson(request, protocol.ConfirmResponse(requestHandler.confirm(executionId, msg)))
          catch {
            case NoInteractionException =>
              client.replyJson(request, protocol.ErrorResponse("Unable to handle request: No interaction is defined"))
          }
        case protocol.Envelope(_, requestSerial, r: protocol.Request) =>
          client.replyJson(requestSerial, protocol.ErrorResponse("Unable to handle request: " + r.simpleName))
        // TODO - Deal with other responses...
        case stuff =>
        // TODO - Do something here.
        //System.err.println("Received gunk from the server!: " + stuff)
      }
  }
  thread.start()

  private val requestHandler = new RequestHandler()

  override def isClosed: Boolean = client.isClosed

}

class RequestException(msg: String) extends Exception(msg)
/** Abstracted mechanism of sending events. */
trait ListenerType[Event] {
  def send(e: Event): Unit
  def onClose(): Unit = {}
}
/** Helper to manage registering events and telling the server we want them. */
private abstract class ListenerManager[Event, Listener, RequestMsg <: Request, UnlistenMsg <: Request](requestEventsMsg: RequestMsg, requestUnlistenMsg: UnlistenMsg, client: ipc.Peer)(implicit format1: play.api.libs.json.Format[RequestMsg], format2: play.api.libs.json.Format[UnlistenMsg])
  extends Closeable {

  def wrapListener(l: Listener, ex: ExecutionContext): ListenerType[Event]

  private val listeningToEvents = new AtomicBoolean(false)
  private var listeners: Set[ListenerType[Event]] = Set.empty
  private var closed = false

  private def sendJson[T: Format](message: T): Unit =
    // don't check the closed flag here, would be a race
    try client.sendJson(message, client.serialGetAndIncrement())
    catch {
      case e: SocketException =>
      // if the socket is closed, just ignore it... close()
      // will be called by the client to clear out
      // and notify our listeners.
      // Note that we may be INSIDE a call to close()
      // right now since close() removes listeners
    }

  def watch(listener: Listener)(implicit ex: ExecutionContext): Subscription = {
    val helper = wrapListener(listener, ex)
    addEventListener(helper)
    object subscription extends Subscription {
      def cancel(): Unit =
        removeEventListener(helper)
    }
    subscription
  }

  private def addEventListener(l: ListenerType[Event]): Unit = synchronized {
    if (closed) {
      // guarantee that listeners always get a ClosedEvent
      l.onClose()
    } else {
      listeners += l
      if (listeningToEvents.compareAndSet(false, true)) {
        sendJson(requestEventsMsg)
      }
    }
  }
  private def removeEventListener(l: ListenerType[Event]): Unit = synchronized {
    listeners -= l
    if (listeners.isEmpty && listeningToEvents.compareAndSet(true, false)) {
      sendJson(requestUnlistenMsg)
    }
  }
  def sendEvent(e: Event): Unit = synchronized {
    listeners foreach { l =>
      try l send e
      catch {
        case NonFatal(_) => // Ignore non fatal exceptions while sending events.
      }
    }
  }

  override def close(): Unit = synchronized {
    if (!closed) {
      closed = true
      listeners foreach { l =>
        try l.onClose()
        catch {
          case NonFatal(_) => // Ignore non fatal exceptions from callbacks
        }
      }
      while (listeners.nonEmpty)
        removeEventListener(listeners.head)
    }
  }
}

/** A wrapped event listener that ensures events are fired on the desired execution context. */
private[client] class EventListenerHelper(listener: EventListener, ex: ExecutionContext) extends ListenerType[Event] {
  private val id = java.util.UUID.randomUUID
  override def send(e: Event): Unit = {
    // TODO - do we need to prepare the context?
    ex.prepare.execute(new Runnable() {
      def run(): Unit = {
        listener(e)
      }
    })
  }
  override def onClose(): Unit = {
    send(ClosedEvent())
  }
  override def hashCode = id.hashCode
  override def equals(o: Any): Boolean = o match {
    case x: EventListenerHelper => x.id == id
    case _ => false
  }

}
/** A wrapped build event listener that ensures events are fired on the desired execution context. */
private[client] class BuildListenerHelper(listener: BuildStructureListener, ex: ExecutionContext) extends ListenerType[MinimalBuildStructure] {
  private val id = java.util.UUID.randomUUID
  override def send(e: MinimalBuildStructure): Unit = {
    // TODO - do we need to prepare the context?
    ex.prepare.execute(new Runnable() {
      def run(): Unit = {
        listener(e)
      }
    })
  }
  override def hashCode = id.hashCode
  override def equals(o: Any): Boolean = o match {
    case x: BuildListenerHelper => x.id == id
    case _ => false
  }
}

/** A wrapped build event listener that ensures events are fired on the desired execution context. */
private[client] class ValueChangeListenerHelper[T](listener: ValueListener[T], ex: ExecutionContext) extends ListenerType[ValueChanged[T]] {
  private val id = java.util.UUID.randomUUID
  override def send(e: ValueChanged[T]): Unit = {
    // TODO - do we need to prepare the context?
    ex.prepare.execute(new Runnable() {
      def run(): Unit = {
        listener(e.key, e.value)
      }
    })
  }
  override def hashCode = id.hashCode
  override def equals(o: Any): Boolean = o match {
    case x: ValueChangeListenerHelper[_] => x.id == id
    case _ => false
  }
}
/** Helper to track value changes. */
private final class ValueChangeManager[T](key: ScopedKey, peer: ipc.Peer)
  extends ListenerManager[ValueChanged[T], ValueListener[T], ListenToValue, UnlistenToValue](ListenToValue(key), UnlistenToValue(key), peer) {

  def wrapListener(l: ValueListener[T], ex: ExecutionContext): ListenerType[ValueChanged[T]] =
    new ValueChangeListenerHelper(l, ex)
}