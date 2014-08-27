package com.typesafe.sbtrc
package client

import sbt.protocol
import sbt.protocol._
import sbt.client.{ SbtChannel, SbtClient, Subscription, BuildStructureListener, EventListener, ValueListener, SettingKey, TaskKey, Interaction }
import scala.concurrent.{ ExecutionContext, Future, Promise }
import java.net.SocketException
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.control.NonFatal
import java.io.IOException
import java.io.EOFException
import java.io.Closeable
import play.api.libs.json.Writes
import scala.annotation.tailrec

/**
 * A concrete implementation of the SbtClient trait.
 */
final class SimpleSbtClient(override val channel: SbtChannel, serializations: ReadOnlyDynamicSerialization) extends SbtClient {

  override def uuid: java.util.UUID = channel.uuid
  override def configName: String = channel.configName
  override def humanReadableName: String = channel.humanReadableName

  def watchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription = {
    val sub = buildEventManager.watch(listener)(ex)
    // TODO this is really busted; we need to have a local cache of the latest build and provide
    // that local cache ONLY to the new listener, rather than reloading remotely and sending
    // it to ALL existing listeners.
    channel.sendJson(SendSyntheticBuildChanged())
    sub
  }

  def lazyWatchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription =
    buildEventManager.watch(listener)(ex)

  private implicit object completionsManager extends RequestManager[CommandCompletionsRequest] {
    override type R = Set[Completion]
  }

  private implicit object cancelRequestManager extends RequestManager[CancelExecutionRequest] {
    override type R = Boolean
  }

  private implicit object keyLookupRequestManager extends RequestManager[KeyLookupRequest] {
    override type R = Seq[ScopedKey]
  }

  private implicit object analyzeExecutionRequestManager extends RequestManager[AnalyzeExecutionRequest] {
    override type R = ExecutionAnalysis
  }

  private def sendRequestWithManager[Req <: Request](request: Req)(implicit manager: RequestManager[Req], writes: Writes[Req]): Future[manager.R] = {
    channel.sendJsonWithRegistration(request) { serial => manager.register(serial) }
  }

  def possibleAutocompletions(partialCommand: String, detailLevel: Int): Future[Set[Completion]] =
    sendRequestWithManager(CommandCompletionsRequest(partialCommand, detailLevel))

  def lookupScopedKey(name: String): Future[Seq[ScopedKey]] =
    sendRequestWithManager(KeyLookupRequest(name))

  def analyzeExecution(command: String): Future[ExecutionAnalysis] =
    sendRequestWithManager(AnalyzeExecutionRequest(command))

  def requestExecution(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] = {
    channel.sendJsonWithRegistration(ExecutionRequest(commandOrTask)) { serial =>
      requestHandler.register(serial, interaction).received
    }
  }
  def requestExecution(key: ScopedKey, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] = {
    channel.sendJsonWithRegistration(KeyExecutionRequest(key)) { serial =>
      requestHandler.register(serial, interaction).received
    }
  }
  def cancelExecution(id: Long): Future[Boolean] =
    sendRequestWithManager(CancelExecutionRequest(id))

  def handleEvents(listener: EventListener)(implicit ex: ExecutionContext): Subscription =
    eventManager.watch(listener)(ex)

  def watch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription = {
    val sub = lazyWatch(key)(listener)
    // TODO this is really busted; we need to have a local cache of the latest value and provide
    // that local cache ONLY to the new listener, rather than reloading remotely and sending
    // it to ALL existing listeners.
    channel.sendJson(SendSyntheticValueChanged(key.key))
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
    channel.sendJson(SendSyntheticValueChanged(key.key))
    sub
  }
  def lazyWatch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit ex: ExecutionContext): Subscription =
    valueEventManager[T](key.key).watch(listener)(ex)

  // TODO - Maybe we should try a bit harder here to `kill` the server.
  // TODO this should be dropped because requestExecution("exit") now does
  // the same thing; we could replace this with a force-kill thing if we want
  // that actually kills the pid or something.
  override def requestSelfDestruct(): Unit =
    channel.sendJson(KillServerRequest())

  private val closeLatch = new java.util.concurrent.CountDownLatch(1)
  def close(): Unit = {
    channel.close()
    closeLatch.await()
  }

  private object eventManager extends ListenerManager[protocol.Event, EventListener, ListenToEvents, UnlistenToEvents](ListenToEvents(), UnlistenToEvents(), channel) {
    override def wrapListener(l: EventListener, ex: ExecutionContext) = new EventListenerHelper(l, ex)
  }
  private object buildEventManager extends ListenerManager[MinimalBuildStructure, BuildStructureListener, ListenToBuildChange, UnlistenToBuildChange](ListenToBuildChange(), UnlistenToBuildChange(), channel) {
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
            new ValueChangeManager[Any](key, channel).asInstanceOf[ValueChangeManager[T]]
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

  private class RequestManager[For <: Request] extends Closeable {
    type R
    private var handlers: Map[Long, Promise[R]] = Map.empty
    def register(serial: Long): Future[R] = synchronized {
      val listener = Promise[R]()
      handlers += (serial -> listener)
      listener.future
    }
    def fire(serial: Long, result: R): Unit = synchronized {
      handlers get serial match {
        case Some(handler) =>
          handler.success(result)
          handlers -= serial
        case None =>
          System.err.println(s"Received an unexpected reply to serial ${serial}")
      }
    }
    override def close(): Unit = synchronized { completePromisesOnClose(handlers) }
  }

  private var executionState: ImpliedState.ExecutionEngine = ImpliedState.ExecutionEngine.empty

  // synchronizing this probably does very little since we are always
  // called from the same channel thread anyhow, but just to be safe
  // we do it.
  private def onMessage(envelope: Envelope): Unit = synchronized {
    val closed = {
      executionState = handleEnvelope(executionState, envelope)
      envelope match {
        case protocol.Envelope(_, _, _: ClosedEvent) => true
        case _ => false
      }
    }

    if (closed) {
      // generate events to terminate any tasks and executions
      for {
        withWrites <- ImpliedState.eventsToEmptyEngineState(executionState, success = false)
        event = withWrites.event
      } {
        executionState = handleEvent(executionState, event)
        ()
      }

      // shut 'er down.
      eventManager.close() // sends ClosedEvent so do this first
      valueEventManager.close()
      buildEventManager.close()
      requestHandler.close()
      completionsManager.close()
      keyLookupRequestManager.close()
      analyzeExecutionRequestManager.close()
      cancelRequestManager.close()

      // notify close() that it can return
      closeLatch.countDown()
    }
  }

  private def handleEvent(executionState: ImpliedState.ExecutionEngine, event: Event): ImpliedState.ExecutionEngine = event match {
    case e: ValueChanged[_] =>
      valueEventManager(e.key).sendEvent(e)
      executionState
    case e: BuildStructureChanged =>
      buildEventManager.sendEvent(e.structure)
      executionState
    case e: protocol.ExecutionSuccess =>
      requestHandler.executionDone(e.id)
      eventManager.sendEvent(e)
      ImpliedState.processEvent(executionState, e)
    case e: protocol.ExecutionFailure =>
      requestHandler.executionFailed(e.id, s"execution failed")
      eventManager.sendEvent(e)
      ImpliedState.processEvent(executionState, e)
    case e: protocol.ExecutionEngineEvent =>
      eventManager.sendEvent(e)
      ImpliedState.processEvent(executionState, e)
    case e: protocol.ExecutionWaiting =>
      eventManager.sendEvent(e)
      ImpliedState.processEvent(executionState, e)
    // We do NOT send ClosedEvent to eventManager because we need
    // to generate synthetic events first from the executionState.
    // We synthesize ClosedEvent ourselves later.
    // So do nothing here (but it will exit our thread)
    case e: ClosedEvent =>
      executionState
    case other =>
      eventManager.sendEvent(other)
      executionState
  }
  private def handleResponse(replyTo: Long, response: Response): Unit = response match {
    case KeyLookupResponse(key, result) =>
      keyLookupRequestManager.fire(replyTo, result)
    case AnalyzeExecutionResponse(analysis) =>
      analyzeExecutionRequestManager.fire(replyTo, analysis)
    case protocol.CommandCompletionsResponse(completions) =>
      completionsManager.fire(replyTo, completions)
    case protocol.ExecutionRequestReceived(executionId) =>
      requestHandler.executionReceived(replyTo, executionId)
    case protocol.CancelExecutionResponse(result) =>
      cancelRequestManager.fire(replyTo, result)
    case protocol.ErrorResponse(msg) =>
      requestHandler.protocolError(replyTo, msg)
    case other =>
    // do nothing, we don't understand it
  }
  private def handleRequest(serial: Long, request: Request): Unit = request match {
    case protocol.ReadLineRequest(executionId, prompt, mask) =>
      try channel.replyJson(serial, protocol.ReadLineResponse(requestHandler.readLine(executionId, prompt, mask)))
      catch {
        case NoInteractionException =>
          channel.replyJson(serial, protocol.ErrorResponse("Unable to handle request: No interaction is defined"))
      }
    case protocol.ConfirmRequest(executionId, msg) =>
      try channel.replyJson(serial, protocol.ConfirmResponse(requestHandler.confirm(executionId, msg)))
      catch {
        case NoInteractionException =>
          channel.replyJson(serial, protocol.ErrorResponse("Unable to handle request: No interaction is defined"))
      }
    case other =>
      channel.replyJson(serial, protocol.ErrorResponse("Unable to handle request: " + request.simpleName))
  }
  private def handleEnvelope(executionState: ImpliedState.ExecutionEngine, envelope: protocol.Envelope): ImpliedState.ExecutionEngine = envelope match {
    case protocol.Envelope(_, _, event: Event) =>
      handleEvent(executionState, event)
    case protocol.Envelope(_, replyTo, response: Response) =>
      handleResponse(replyTo, response)
      executionState
    case protocol.Envelope(serial, _, request: Request) =>
      handleRequest(serial, request)
      executionState
  }

  private val requestHandler = new RequestHandler()

  override def isClosed: Boolean = channel.isClosed

  // this is mildly dangerous but we know we are called from the
  // SimpleChannel thread unless the channel is already closed,
  // and we can just make that thread safe to invoke listeners
  // from (they all have their own EC anyhow)
  private object RunOnSameThreadContext extends ExecutionContext {
    def execute(runnable: Runnable): Unit = runnable.run()
    def reportFailure(t: Throwable): Unit = ()
  }

  // This throws if we wrap the same channel twice. It's here
  // at the bottom of initialization since we don't want onMessage
  // called before we fill in all of our fields. If the channel
  // is already closed on client side (which should not be possible since
  // we didn't close it) then we would in theory get a synchronous ClosedEvent
  // here.
  channel.claimMessages(onMessage, serializations)(RunOnSameThreadContext)
}

class RequestException(msg: String) extends Exception(msg)
/** Abstracted mechanism of sending events. */
trait ListenerType[Event] {
  def send(e: Event): Unit
  def onClose(): Unit = {}
}
/** Helper to manage registering events and telling the server we want them. */
private abstract class ListenerManager[Event, Listener, RequestMsg <: Request, UnlistenMsg <: Request](requestEventsMsg: RequestMsg, requestUnlistenMsg: UnlistenMsg, channel: SbtChannel)(implicit format1: play.api.libs.json.Format[RequestMsg], format2: play.api.libs.json.Format[UnlistenMsg])
  extends Closeable {

  def wrapListener(l: Listener, ex: ExecutionContext): ListenerType[Event]

  private val listeningToEvents = new AtomicBoolean(false)
  private var listeners: Set[ListenerType[Event]] = Set.empty
  private var closed = false

  private def sendJson[T: Writes](message: T): Unit =
    // don't check the closed flag here, would be a race.
    // we fire-and-forget the returned future.
    channel.sendJson(message)

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
private final class ValueChangeManager[T](key: ScopedKey, channel: SbtChannel)
  extends ListenerManager[ValueChanged[T], ValueListener[T], ListenToValue, UnlistenToValue](ListenToValue(key), UnlistenToValue(key), channel) {

  def wrapListener(l: ValueListener[T], ex: ExecutionContext): ListenerType[ValueChanged[T]] =
    new ValueChangeListenerHelper(l, ex)
}
