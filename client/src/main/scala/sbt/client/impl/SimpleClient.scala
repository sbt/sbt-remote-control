
package sbt.client.impl

import sbt.protocol
import sbt.protocol._
import sbt.serialization._
import sbt.client.{ SbtChannel, SbtClient, Subscription, BuildStructureListener, EventListener, ValueListener, RawValueListener, SettingKey, TaskKey, Interaction }
import java.net.SocketException
import java.util.concurrent.atomic.AtomicBoolean
import java.io.IOException
import java.io.EOFException
import java.io.Closeable
import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.util.{ Success, Failure, Try }
import scala.util.control.NonFatal

/**
 * A concrete implementation of the SbtClient trait.
 */
private[client] final class SimpleSbtClient(override val channel: SbtChannel) extends SbtClient {
  import sbt.serialization._

  override def uuid: java.util.UUID = channel.uuid
  override def configName: String = channel.configName
  override def humanReadableName: String = channel.humanReadableName
  override def serverProtocolVersion: protocol.ProtocolVersion = channel.serverProtocolVersion
  override def serverTags: Seq[protocol.FeatureTag] = channel.serverTags

  def watchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription = {
    val sub = buildEventManager.watch(listener)(ex)

    // TODO this is really busted; we need to have a local cache of the latest build and provide
    // that local cache ONLY to the new listener, rather than reloading remotely and sending
    // it to ALL existing listeners.
    channel.sendMessage(SendSyntheticBuildChanged())
    sub
  }

  def lazyWatchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription =
    buildEventManager.watch(listener)(ex)

  private abstract class AckResponseConverter[R <: Request] extends ResponseConverter[R, Unit] {
    override def convert: Converter = {
      case protocol.ReceivedResponse() =>
        Success(())
    }
  }

  private implicit object daemonResponseConverter extends AckResponseConverter[DaemonRequest]

  private implicit object completionsResponseConverter extends ResponseConverter[CommandCompletionsRequest, Vector[Completion]] {
    override def convert: Converter = {
      case protocol.CommandCompletionsResponse(completions) => Success(completions)
    }
  }

  private implicit object cancelResponseConverter extends ResponseConverter[CancelExecutionRequest, Boolean] {
    override def convert: Converter = {
      case protocol.CancelExecutionResponse(result) => Success(result)
    }
  }

  private implicit object keyLookupResponseConverter extends ResponseConverter[KeyLookupRequest, Seq[ScopedKey]] {
    override def convert: Converter = {
      case protocol.KeyLookupResponse(key, result) => Success(result)
    }
  }

  private implicit object listSettingsResponseConverter extends ResponseConverter[ListSettingsRequest, Seq[ScopedKey]] {
    override def convert: Converter = {
      case ListSettingsResponse(settings) => Success(settings)
    }
  }

  private implicit object inspectResponseConverter extends ResponseConverter[InspectRequest, InspectResponse] {
    override def convert: Converter = {
      case response: protocol.InspectResponse => Success(response)
    }
  }

  private implicit object analyzeExecutionResponseConverter extends ResponseConverter[AnalyzeExecutionRequest, ExecutionAnalysis] {
    override def convert: Converter = {
      case protocol.AnalyzeExecutionResponse(analysis) => Success(analysis)
    }
  }

  private implicit object sendSyntheticValueChangedResponseConverter extends ResponseConverter[SendSyntheticValueChanged, Unit] {
    override def convert: Converter = {
      case protocol.ReceivedResponse() =>
        Success(())
      case protocol.KeyNotFound(key) =>
        Failure(new Exception(s"No key $key exists, cannot get its value"))
    }
  }

  private def sendRequestWithConverter[Req <: Request, R](request: Req)(implicit converter: ResponseConverter[Req, R]): Future[R] = {
    channel.sendMessageWithRegistration(request) { serial => responseTracker.register[Req, R](serial) }
  }

  override def setDaemon(daemon: Boolean): Future[Unit] = {
    sendRequestWithConverter(DaemonRequest(daemon))
  }

  def possibleAutocompletions(partialCommand: String, detailLevel: Int): Future[Vector[Completion]] =
    sendRequestWithConverter(CommandCompletionsRequest(partialCommand, detailLevel))

  def lookupScopedKey(name: String): Future[Seq[ScopedKey]] =
    sendRequestWithConverter(KeyLookupRequest(name))

  def listSettings(): Future[Seq[ScopedKey]] =
    sendRequestWithConverter(ListSettingsRequest())

  def inspectKey(key: ScopedKey, preanalyze: Boolean): Future[InspectResponse] =
    sendRequestWithConverter(InspectRequest(key, preanalyze))

  def analyzeExecution(command: String): Future[ExecutionAnalysis] =
    sendRequestWithConverter(AnalyzeExecutionRequest(command))

  def requestExecution(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] = {
    channel.sendMessageWithRegistration(ExecutionRequest(commandOrTask)) { serial =>
      requestHandler.register(serial, interaction).received
    }
  }
  def requestExecution(key: ScopedKey, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] = {
    channel.sendMessageWithRegistration(KeyExecutionRequest(key)) { serial =>
      requestHandler.register(serial, interaction).received
    }
  }
  def cancelExecution(id: Long): Future[Boolean] =
    sendRequestWithConverter(CancelExecutionRequest(id))

  def handleEvents(listener: EventListener)(implicit ex: ExecutionContext): Subscription =
    eventManager.watch(listener)(ex)

  // TODO this is really busted; we need to have a local cache of the latest value and provide
  // that local cache ONLY to the new listener, rather than reloading remotely and sending
  // it to ALL existing listeners.
  private def requestSyntheticValueChanged(key: protocol.ScopedKey)(implicit ex: ExecutionContext): Unit = {
    // Right now, anytime we add a listener to a task we re-run that task (!!!)
    // Combined with the issue of adding interaction handlers, having watch() on tasks
    // do a notification right away may simply be a bad idea?
    sendRequestWithConverter(SendSyntheticValueChanged(key)) onFailure {
      case e: Throwable =>
        valueEventManager(key).sendEvent(ValueChanged(key, TaskFailure(BuildValue(e))))
    }
  }

  def rawWatch(key: SettingKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription = {
    val sub = rawLazyWatch(key)(listener)
    requestSyntheticValueChanged(key.key)
    sub
  }
  def rawLazyWatch(key: SettingKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    valueEventManager(key.key).watch(listener)(ex)

  // TODO - Some mechanisms of listening to interaction here...
  def rawWatch(key: TaskKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription = {
    val sub = rawLazyWatch(key)(listener)
    requestSyntheticValueChanged(key.key)
    sub
  }
  def rawLazyWatch(key: TaskKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    valueEventManager(key.key).watch(listener)(ex)

  private def toRaw[T](listener: ValueListener[T])(implicit unpickler: Unpickler[T]): RawValueListener = (key: ScopedKey, taskResult: TaskResult) =>
    listener(key, taskResult.result[T])

  def watch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    rawWatch(key)(toRaw(listener))

  def lazyWatch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    rawLazyWatch(key)(toRaw(listener))

  def watch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    rawWatch(key)(toRaw(listener))

  def lazyWatch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    rawLazyWatch(key)(toRaw(listener))

  def watch[T](name: String)(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    rawWatch(name)(toRaw(listener))

  def lazyWatch[T](name: String)(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    rawLazyWatch(name)(toRaw(listener))

  def rawWatch(name: String)(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    nameWatchManager.watch(name, isLazy = false)(listener)

  def rawLazyWatch(name: String)(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    nameWatchManager.watch(name, isLazy = true)(listener)

  private object nameWatchManager {
    private case class Registered(name: String, isLazy: Boolean, currentSubs: List[Subscription],
      listener: RawValueListener, executor: ExecutionContext)

    private var buildWatch: Option[Subscription] = None
    private val nextId = new java.util.concurrent.atomic.AtomicLong(1)
    // these are the states a Registered can be in; each id may be in only one of these at a time.
    // Access to them must be synchronized.
    private var awaitingLookup = Map.empty[Long, Registered]
    private var needsRelookup = Map.empty[Long, Registered]
    private var currentlyWatching = Map.empty[Long, Registered]

    private def updateBuildWatch(): Unit = synchronized {
      val needBuildWatch = awaitingLookup.nonEmpty || needsRelookup.nonEmpty || currentlyWatching.nonEmpty
      buildWatch match {
        case Some(sub) => if (!needBuildWatch) {
          sub.cancel()
          buildWatch = None
        }
        case None => if (needBuildWatch) {
          import scala.concurrent.ExecutionContext.Implicits.global
          buildWatch = Some(lazyWatchBuild(_ => reloadAll()))
        }
      }
    }

    private def successfulLookup(id: Long, registered: Registered, scopeds: Seq[ScopedKey]): Unit = synchronized {
      // if we were canceled, we won't be in awaitingLookup anymore
      if (awaitingLookup.contains(id)) {
        val subs = scopeds map { scoped =>
          if (registered.isLazy)
            rawLazyWatch(TaskKey[Any](scoped))(registered.listener)(registered.executor)
          else
            rawWatch(TaskKey[Any](scoped))(registered.listener)(registered.executor)
        }
        awaitingLookup -= id
        currentlyWatching += (id -> registered.copy(currentSubs = subs.toList))
      } else if (needsRelookup.contains(id)) {
        needsRelookup -= id
        startLookup(id, registered)
      }
    }

    private def failedLookup(id: Long, registered: Registered, cause: Throwable): Unit = synchronized {
      if (awaitingLookup.contains(id)) {
        // Key does not currently exist; notify one failure, to
        // keep the invariant that if you watch you always get at
        // least one callback
        registered.executor.execute(new Runnable() {
          override def run(): Unit = {
            val genericKey =
              ScopedKey(AttributeKey(registered.name, TypeExpression("java.lang.Object", Nil)), SbtScope())
            val noKeyResult = TaskFailure(BuildValue(cause))
            registered.listener.apply(genericKey, noKeyResult)
          }
        })
        awaitingLookup -= id
        currentlyWatching += (id -> registered.copy(currentSubs = Nil))
      } else if (needsRelookup.contains(id)) {
        needsRelookup -= id
        startLookup(id, registered)
      }
    }

    private def startLookup(id: Long, registered: Registered): Unit = {
      synchronized {
        needsRelookup -= id // may or may not be in there already
        awaitingLookup += (id -> registered)
        updateBuildWatch()
      }
      import scala.concurrent.ExecutionContext.Implicits.global
      lookupScopedKey(registered.name) map { scopeds =>
        if (scopeds.nonEmpty)
          successfulLookup(id, registered, scopeds)
        else
          failedLookup(id, registered, new Exception(s"No tasks found matching '${registered.name}'"))
      } recover {
        case t: Throwable =>
          failedLookup(id, registered, t)
      }
    }

    private def cancel(id: Long): Unit = synchronized {
      if (awaitingLookup.contains(id)) {
        awaitingLookup -= id
      } else if (needsRelookup.contains(id)) {
        needsRelookup -= id
      } else if (currentlyWatching.contains(id)) {
        val registered = currentlyWatching(id)
        currentlyWatching -= id
        registered.currentSubs.foreach(_.cancel())
      }
      updateBuildWatch()
    }

    def watch(name: String, isLazy: Boolean)(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription = {
      val id = nextId.getAndIncrement()
      val registered = Registered(name, isLazy, currentSubs = Nil, listener = listener, executor = ex)

      startLookup(id, registered)

      new Subscription() {
        override def cancel(): Unit = {
          nameWatchManager.cancel(id)
        }
      }
    }

    // on build structure changed we call this to reset everything
    private def reloadAll(): Unit = synchronized {
      // move pending lookups into needsRelookup
      val waiting = awaitingLookup.toList
      for (pair <- waiting) {
        awaitingLookup -= pair._1
        needsRelookup += pair
      }
      // cancel current watches and look them up again
      val current = currentlyWatching.toList
      for (pair <- current) {
        pair._2.currentSubs.foreach(_.cancel())
        currentlyWatching -= pair._1
        needsRelookup += pair
      }
      val relookup = needsRelookup.toList
      for (pair <- relookup) {
        startLookup(pair._1, pair._2)
      }
    }
  }

  // TODO - Maybe we should try a bit harder here to `kill` the server.
  // TODO this should be dropped because requestExecution("exit") now does
  // the same thing; we could replace this with a force-kill thing if we want
  // that actually kills the pid or something.
  override def requestSelfDestruct(): Unit =
    channel.sendMessage(KillServerRequest())

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
    private var valueListeners = collection.mutable.Map.empty[ScopedKey, ValueChangeManager]

    def apply(key: ScopedKey): ValueChangeManager = synchronized {
      valueListeners.get(key) match {
        case Some(mgr) => mgr
        case None =>
          val mgr =
            new ValueChangeManager(key, channel)
          valueListeners.put(key, mgr)
          mgr
      }
    }
    override def close(): Unit = {
      valueListeners.values.foreach(_.close())
    }
  }
  def completePromisesOnClose(handlers: Iterable[Promise[_]]): Unit = {
    for (promise <- handlers)
      promise.failure(new RuntimeException("Connection to sbt closed"))
  }

  private abstract class ResponseConverter[For <: Request, Result] {
    type Converter = PartialFunction[Response, Try[Result]]
    def convert: Converter
    private val realConvert = convert
    final def complete(response: Response, promise: Promise[Result]): Unit = response match {
      case ErrorResponse(error) =>
        promise.tryFailure(new Exception(error))
      case other =>
        val result = realConvert.applyOrElse(other,
          { _: Response => Failure(new Exception(s"Response $other not handled by ${this.getClass.getName}")) })
        promise.tryComplete(result)
    }
  }

  private final class ResponseHandler[Req <: Request, Result](val converter: ResponseConverter[Req, Result]) {
    val promise = Promise[Result]()
    def future: Future[Result] = promise.future
    def fire(response: Response): Unit = {
      converter.complete(response, promise)
    }
  }

  private object responseTracker extends Closeable {
    private var handlers: Map[Long, ResponseHandler[_, _]] = Map.empty
    def register[Req <: Request, R](serial: Long)(implicit converter: ResponseConverter[Req, R]): Future[R] = synchronized {
      require(!handlers.contains(serial))
      val listener = new ResponseHandler[Req, R](converter)
      handlers += (serial -> listener)
      listener.future
    }
    // returns true if the response was handled
    def fire(serial: Long, response: Response): Boolean = synchronized {
      handlers get serial match {
        case Some(handler) =>
          handler.fire(response)
          handlers -= serial
          true
        case None =>
          false
      }
    }
    override def close(): Unit = synchronized { completePromisesOnClose(handlers.values.map(_.promise)) }
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
      responseTracker.close()

      // notify close() that it can return
      closeLatch.countDown()
    }
  }

  private def handleEvent(executionState: ImpliedState.ExecutionEngine, event: Event): ImpliedState.ExecutionEngine = event match {
    case e: ValueChanged =>
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
  private def handleResponse(replyTo: Long, response: Response): Unit = {
    // First try our generic system
    if (!responseTracker.fire(replyTo, response)) {
      // Then try some special-cases that should probably be
      // moved to the above generic system. FIXME move these
      response match {
        case protocol.ExecutionRequestReceived(executionId) =>
          requestHandler.executionReceived(replyTo, executionId)
        case protocol.ErrorResponse(msg) =>
          requestHandler.protocolError(replyTo, msg)
        case protocol.ReceivedResponse() => // silently ignore
        case _ =>
          if (System.getProperty("sbt.client.debug") == "true")
            System.err.println(s"Unhandled response $replyTo $response")
      }
    }
  }
  private def handleRequest(serial: Long, request: Request): Unit = request match {
    case protocol.ReadLineRequest(executionId, prompt, mask) =>
      try channel.replyMessage(serial, protocol.ReadLineResponse(requestHandler.readLine(executionId, prompt, mask)))
      catch {
        case NoInteractionException =>
          channel.replyMessage(serial, protocol.ErrorResponse("Unable to handle request: No interaction is defined"))
      }
    case protocol.ConfirmRequest(executionId, msg) =>
      try channel.replyMessage(serial, protocol.ConfirmResponse(requestHandler.confirm(executionId, msg)))
      catch {
        case NoInteractionException =>
          channel.replyMessage(serial, protocol.ErrorResponse("Unable to handle request: No interaction is defined"))
      }
    case other =>
      channel.replyMessage(serial, protocol.ErrorResponse("Unable to handle request: " + request.getClass.getName))
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

  // FIXME this is only for RequestExecution requests, not
  // all requests, so is misnamed.
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
  channel.claimMessages(onMessage)(RunOnSameThreadContext)
}

private[client] class RequestException(msg: String) extends Exception(msg)
/** Abstracted mechanism of sending events. */
private[client] trait ListenerType[Event] {
  def send(e: Event): Unit
  def onClose(): Unit = {}
}
/** Helper to manage registering events and telling the server we want them. */
private abstract class ListenerManager[Event, Listener, RequestMsg <: Request, UnlistenMsg <: Request](requestEventsMsg: RequestMsg, requestUnlistenMsg: UnlistenMsg, channel: SbtChannel)
  extends Closeable {

  def wrapListener(l: Listener, ex: ExecutionContext): ListenerType[Event]

  private val listeningToEvents = new AtomicBoolean(false)
  private var listeners: Set[ListenerType[Event]] = Set.empty
  private var closed = false

  private def sendMessage(message: Message): Unit =
    // don't check the closed flag here, would be a race.
    // we fire-and-forget the returned future.
    channel.sendMessage(message)

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
        // TODO this needs to go through the sendRequestWithRegistration stuff
        // above so that we track responses and get any error message properly.
        // Specifically ListenToValue needs to handle KeyNotFound.
        // Not doing it right now because it creates issues with merging
        // branches.
        sendMessage(requestEventsMsg)
      }
    }
  }
  private def removeEventListener(l: ListenerType[Event]): Unit = synchronized {
    listeners -= l
    if (listeners.isEmpty && listeningToEvents.compareAndSet(true, false)) {
      sendMessage(requestUnlistenMsg)
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
private[client] class ValueChangeListenerHelper(listener: RawValueListener, ex: ExecutionContext) extends ListenerType[ValueChanged] {
  private val id = java.util.UUID.randomUUID
  override def send(e: ValueChanged): Unit = {
    // TODO - do we need to prepare the context?
    ex.prepare.execute(new Runnable() {
      def run(): Unit = {
        listener(e.key, e.value)
      }
    })
  }
  override def hashCode = id.hashCode
  override def equals(o: Any): Boolean = o match {
    case x: ValueChangeListenerHelper => x.id == id
    case _ => false
  }
}
/** Helper to track value changes. */
private final class ValueChangeManager(key: ScopedKey, channel: SbtChannel)
  extends ListenerManager[ValueChanged, RawValueListener, ListenToValue, UnlistenToValue](ListenToValue(key), UnlistenToValue(key), channel) {

  def wrapListener(l: RawValueListener, ex: ExecutionContext): ListenerType[ValueChanged] =
    new ValueChangeListenerHelper(l, ex)
}
