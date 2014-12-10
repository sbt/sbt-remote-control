/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import akka.actor._
import java.io.File
import sbt.client.{ Subscription, SbtConnector, SbtClient, Interaction, SettingKey, TaskKey }
import scala.concurrent.ExecutionContext
import scala.util.{ Try, Success, Failure }
import sbt.protocol.{ Analysis, CompileFailedException, TaskResult, ScopedKey, BuildValue, CompilationFailure }
import sbt.protocol
import scala.language.existentials, scala.language.higherKinds
import scala.collection.TraversableOnce

object SbtClientProxy {
  sealed trait Notification
  object Notifications {
    case object Reconnected extends Notification
    case object HandledTermination extends Notification
  }

  case class UpdateClient(client: SbtClient)
  case class WatchEvent(key: protocol.ScopedKey, result: protocol.TaskResult)

  sealed trait Response
  case class LookupScopedKeyResponse(name: String, result: Try[Seq[ScopedKey]]) extends Response
  case class ExecutionId(id: Try[Long], execution: RequestExecution) extends Response
  case class CancelExecutionResponse(id: Long, result: Try[Boolean]) extends Response
  case object EventsSubscribed extends Response
  case object EventsUnsubscribed extends Response
  case object BuildSubscribed extends Response
  case object BuildUnsubscribed extends Response
  case class WatchingTask(key: TaskKey[_]) extends Response
  case class TaskWatchRemoved(key: TaskKey[_]) extends Response
  case class WatchingSetting(key: SettingKey[_]) extends Response
  case class SettingWatchRemoved(key: SettingKey[_]) extends Response
  case object Closed extends Response

  sealed trait LocalRequest[Resp] extends Request[Resp]
  case class LookupScopedKey(name: String, sendTo: ActorRef) extends LocalRequest[LookupScopedKeyResponse] {
    def responseWithResult(result: Try[Seq[ScopedKey]])(implicit sender: ActorRef): Unit = response(LookupScopedKeyResponse(name, result))
  }

  sealed trait RequestExecution extends LocalRequest[ExecutionId] {
    def interaction: Option[(Interaction, ExecutionContext)]
    final def responseWithExecutionId(id: Try[Long])(implicit sender: ActorRef): Unit = response(ExecutionId(id, this))
  }
  object RequestExecution {
    case class ByCommandOrTask(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)], sendTo: ActorRef) extends RequestExecution
    case class ByScopedKey(key: ScopedKey, interaction: Option[(Interaction, ExecutionContext)], sendTo: ActorRef) extends RequestExecution
  }

  case class CancelExecution(id: Long, sendTo: ActorRef) extends LocalRequest[CancelExecutionResponse] {
    final def responseWithResult(r: Try[Boolean])(implicit sender: ActorRef): Unit = response(CancelExecutionResponse(id, r))
  }

  case class SubscribeToEvents(sendTo: ActorRef) extends LocalRequest[EventsSubscribed.type] {
    def subscribed()(implicit sender: ActorRef): Unit = response(EventsSubscribed)
  }
  case class UnsubscribeFromEvents(sendTo: ActorRef) extends LocalRequest[EventsUnsubscribed.type] {
    def unsubscribed()(implicit sender: ActorRef): Unit = response(EventsUnsubscribed)
  }
  case class SubscribeToBuild(sendTo: ActorRef) extends LocalRequest[BuildSubscribed.type] {
    def subscribed()(implicit sender: ActorRef): Unit = response(BuildSubscribed)
  }
  case class UnsubscribeFromBuild(sendTo: ActorRef) extends LocalRequest[BuildUnsubscribed.type] {
    def unsubscribed()(implicit sender: ActorRef): Unit = response(BuildUnsubscribed)
  }
  case class WatchTask(key: TaskKey[_], sendTo: ActorRef) extends LocalRequest[WatchingTask] {
    def watching(key: TaskKey[_])(implicit sender: ActorRef): Unit = response(WatchingTask(key))
  }
  case class RemoveTaskWatch(key: TaskKey[_], sendTo: ActorRef) extends LocalRequest[TaskWatchRemoved] {
    def removed(key: TaskKey[_])(implicit sender: ActorRef): Unit = response(TaskWatchRemoved(key))
  }
  case class WatchSetting(key: SettingKey[_], sendTo: ActorRef) extends LocalRequest[WatchingSetting] {
    def watching(key: SettingKey[_])(implicit sender: ActorRef): Unit = response(WatchingSetting(key))
  }
  case class RemoveSettingWatch(key: SettingKey[_], sendTo: ActorRef) extends LocalRequest[SettingWatchRemoved] {
    def removed(key: SettingKey[_])(implicit sender: ActorRef): Unit = response(SettingWatchRemoved(key))
  }
  case class Close(sendTo: ActorRef) extends LocalRequest[Closed.type] {
    def closed()(implicit sender: ActorRef): Unit = response(Closed)
  }

  sealed trait GenericKey[T] {
    def scopedKey: ScopedKey
  }
  case class GenericSettingKey[T](key: SettingKey[T], manifest: Manifest[T]) extends GenericKey[T] {
    def scopedKey: ScopedKey = key.key
  }
  case class GenericTaskKey[T](key: TaskKey[T], manifest: Manifest[T]) extends GenericKey[T] {
    def scopedKey: ScopedKey = key.key
  }

  object GenericKey {
    def fromSettingKey[T](key: SettingKey[T])(implicit mf: Manifest[T]): GenericKey[T] =
      GenericSettingKey[T](key, mf)
    def fromTaskKey[T](key: TaskKey[T])(implicit mf: Manifest[T]): GenericKey[T] =
      GenericTaskKey[T](key, mf)
  }

  trait SubscriptionHolder[T] {
    def subscription: Subscription
    def subscribers: Set[ActorRef]
    def update(subscription: Subscription = subscription, subscribers: Set[ActorRef] = subscribers): T
    def liftToOption(): Option[T]
    final def addSubscriber(target: ActorRef): T = update(subscribers = subscribers + target)
    final def updateSubscription(subscription: Subscription): T = update(subscription = subscription)
    final def removeSubscriber(target: ActorRef): T = update(subscribers = subscribers - target)
    final def hasSubscribers: Boolean = !subscribers.isEmpty
    final def maybeCancel(): Unit = {
      if (!hasSubscribers) subscription.cancel()
    }
  }

  case class EventSubscribers(subscription: Subscription, subscribers: Set[ActorRef] = Set.empty[ActorRef]) extends SubscriptionHolder[EventSubscribers] {
    def update(subscription: Subscription = subscription, subscribers: Set[ActorRef] = subscribers): EventSubscribers = EventSubscribers(subscription, subscribers)
    final def liftToOption(): Option[EventSubscribers] = {
      maybeCancel()
      if (hasSubscribers) Some(this)
      else None
    }
  }
  case class BuildSubscribers(subscription: Subscription, subscribers: Set[ActorRef] = Set.empty[ActorRef]) extends SubscriptionHolder[BuildSubscribers] {
    def update(subscription: Subscription = subscription, subscribers: Set[ActorRef] = subscribers): BuildSubscribers = BuildSubscribers(subscription, subscribers)
    final def liftToOption(): Option[BuildSubscribers] = {
      maybeCancel()
      if (hasSubscribers) Some(this)
      else None
    }
  }
  case class WatchSubscribers(key: GenericKey[_], subscription: Subscription, subscribers: Set[ActorRef] = Set.empty[ActorRef]) extends SubscriptionHolder[WatchSubscribers] {
    def update(subscription: Subscription = subscription, subscribers: Set[ActorRef] = subscribers): WatchSubscribers =
      this.copy(subscription = subscription, subscribers = subscribers)
    final def liftToOption(): Option[WatchSubscribers] = {
      maybeCancel()
      if (hasSubscribers) Some(this)
      else None
    }
  }

  object SubscriptionHolder {
    def cancelAll(hs: TraversableOnce[SubscriptionHolder[_]]): Unit = hs.foreach(_.subscription.cancel())
  }

  final case class State(client: SbtClient,
    eventSubscriptions: Option[EventSubscribers] = None,
    buildSubscriptions: Option[BuildSubscribers] = None,
    watchSubscriptions: Map[protocol.ScopedKey, WatchSubscribers] = Map.empty[protocol.ScopedKey, WatchSubscribers]) { self =>

    private abstract class Updater[T <: SubscriptionHolder[T]] {
      def initial(sub: Subscription, target: ActorRef): State
      def update(holder: T, target: ActorRef): State
      def remove(target: ActorRef): State
    }

    // Internal typeclasses.  Woo hoo!
    private object EventSubscribersUpdater extends Updater[EventSubscribers] {
      def initial(sub: Subscription, target: ActorRef): State =
        self.updateEventSubscriptions(EventSubscribers(sub, Set(target)))
      def update(holder: EventSubscribers, target: ActorRef): State =
        self.updateEventSubscriptions(holder.addSubscriber(target))
      def remove(target: ActorRef): State =
        self.copy(eventSubscriptions = self.eventSubscriptions.flatMap(_.removeSubscriber(target).liftToOption()))
    }

    private object BuildSubscribersUpdater extends Updater[BuildSubscribers] {
      def initial(sub: Subscription, target: ActorRef): State =
        self.updateBuildSubscriptions(BuildSubscribers(sub, Set(target)))
      def update(holder: BuildSubscribers, target: ActorRef): State =
        self.updateBuildSubscriptions(holder.addSubscriber(target))
      def remove(target: ActorRef): State =
        self.copy(buildSubscriptions = self.buildSubscriptions.flatMap(_.removeSubscriber(target).liftToOption()))
    }

    def allSubscribers(): Set[ActorRef] = {
      (eventSubscriptions.map(_.subscribers) getOrElse Set.empty[ActorRef]) ++
        (buildSubscriptions.map(_.subscribers) getOrElse Set.empty[ActorRef]) ++
        (watchSubscriptions.values.map(_.subscribers).flatten.toSet)
    }
    def updateEventSubscriptions(s: EventSubscribers): State = this.copy(eventSubscriptions = Some(s))
    def updateBuildSubscriptions(s: BuildSubscribers): State = this.copy(buildSubscriptions = Some(s))
    def setWatchSubscriptions(key: protocol.ScopedKey, s: WatchSubscribers): State = this.copy(watchSubscriptions = this.watchSubscriptions + (key -> s))

    def updateSubscription[T <: SubscriptionHolder[T]](target: ActorRef,
      genSubscription: () => Subscription,
      subscriptions: Option[T],
      updater: Updater[T]): State =
      subscriptions match {
        case Some(sh) => updater.update(sh, target)
        case None => updater.initial(genSubscription(), target)
      }

    def resubscribeEventSubscriptions(subs: => Subscription): State =
      this.copy(eventSubscriptions = this.eventSubscriptions.map { x =>
        x.subscription.cancel()
        x.copy(subscription = subs)
      })

    def resubscribeBuildSubscription(subs: => Subscription): State =
      this.copy(buildSubscriptions = this.buildSubscriptions.map { x =>
        x.subscription.cancel()
        x.copy(subscription = subs)
      })

    def resubscribeWatchSubscriptions(subsGen: GenericKey[_] => Subscription): State =
      this.copy(watchSubscriptions = this.watchSubscriptions mapValues { ws =>
        ws.subscription.cancel() // Can this throw an exeption?
        ws.copy(subscription = subsGen(ws.key))
      })

    def addOrUpdateEventSubscriptions(target: ActorRef,
      genSubscription: () => Subscription): State =
      updateSubscription(target, genSubscription, eventSubscriptions, EventSubscribersUpdater)

    def addOrUpdateBuildSubscriptions(target: ActorRef,
      genSubscription: () => Subscription): State =
      updateSubscription(target, genSubscription, buildSubscriptions, BuildSubscribersUpdater)

    def removeEventSubscription(target: ActorRef): State = EventSubscribersUpdater.remove(target)

    def removeBuildSubscription(target: ActorRef): State = BuildSubscribersUpdater.remove(target)

    def addOrUpdateWatchSubscriptions(key: GenericKey[_],
      target: ActorRef,
      genSubscription: () => Subscription): State =
      watchSubscriptions.get(key.scopedKey) match {
        case Some(ws) =>
          val newWatcher = ws.copy(subscribers = ws.subscribers + target)
          this.copy(watchSubscriptions = this.watchSubscriptions + (key.scopedKey -> newWatcher))
        case None =>
          val newWatcher = WatchSubscribers(key, genSubscription(), Set(target))
          this.copy(watchSubscriptions = this.watchSubscriptions + (key.scopedKey -> newWatcher))
      }

    def removeWatchSubscription(key: GenericKey[_], target: ActorRef): State =
      watchSubscriptions.get(key.scopedKey) match {
        case Some(ws) =>
          val newWatcher = ws.removeSubscriber(target)
          newWatcher.maybeCancel()
          if (newWatcher.hasSubscribers) this.copy(watchSubscriptions = this.watchSubscriptions + (key.scopedKey -> newWatcher))
          else this.copy(watchSubscriptions = this.watchSubscriptions - key.scopedKey)
        case None => this
      }

    def removeAllWatchSubscription(target: ActorRef): State = {
      val newWatchSubscriptions = watchSubscriptions.mapValues(_.removeSubscriber(target)) filter {
        case (_, ws) =>
          ws.maybeCancel()
          ws.hasSubscribers
      }
      this.copy(watchSubscriptions = newWatchSubscriptions)
    }

  }

  def props(initialClient: SbtClient,
    notificationSink: SbtClientProxy.Notification => Unit = _ => ())(implicit ex: ExecutionContext): Props =
    Props(new SbtClientProxy(initialClient = initialClient,
      ec = ex,
      notificationSink = notificationSink))
}

final class SbtClientProxy(initialClient: SbtClient,
  ec: ExecutionContext,
  notificationSink: SbtClientProxy.Notification => Unit = _ => ()) extends Actor with ActorLogging {
  import SbtClientProxy._, SubscriptionHolder._
  implicit val ec1 = ec

  private def eventListener(self: ActorRef)(event: protocol.Event): Unit = {
    self ! event
  }

  private def buildListener(self: ActorRef)(event: protocol.MinimalBuildStructure): Unit = {
    self ! event
  }

  private def watchListener(self: ActorRef)(key: protocol.ScopedKey, result: protocol.TaskResult): Unit = {
    self ! WatchEvent(key, result)
  }

  private def canUnwatch(state: State, target: ActorRef): Boolean = state.allSubscribers()(target)

  private def runWith(state: State): Unit =
    context.become(running(state))

  private def onRequest(req: LocalRequest[_], state: State, self: ActorRef): Unit = {
    implicit val localSelf = self

    req match {
      case r: Close =>
        cancelAll(state.eventSubscriptions)
        cancelAll(state.buildSubscriptions)
        cancelAll(state.watchSubscriptions.values)
        state.client.close()
        r.closed()
        context stop self
      case r: LookupScopedKey =>
        state.client.lookupScopedKey(r.name).onComplete(r.responseWithResult)
      case r: RequestExecution.ByCommandOrTask =>
        state.client.requestExecution(r.commandOrTask, r.interaction).onComplete(id => r.responseWithExecutionId(id))
      case r: RequestExecution.ByScopedKey =>
        state.client.requestExecution(r.key, r.interaction).onComplete(id => r.responseWithExecutionId(id))
      case r: CancelExecution =>
        state.client.cancelExecution(r.id).onComplete(x => r.responseWithResult(x))
      case r: SubscribeToEvents =>
        runWith(state.addOrUpdateEventSubscriptions(r.sendTo, () => state.client.handleEvents(eventListener(self))))
        context.watch(r.sendTo)
        r.subscribed()
      case r: UnsubscribeFromEvents =>
        val newState = state.removeEventSubscription(r.sendTo)
        runWith(newState)
        if (canUnwatch(newState, r.sendTo)) context.unwatch(r.sendTo)
        r.unsubscribed()
      case r: SubscribeToBuild =>
        runWith(state.addOrUpdateBuildSubscriptions(r.sendTo, () => state.client.watchBuild(buildListener(self))))
        context.watch(r.sendTo)
        r.subscribed()
      case r: UnsubscribeFromBuild =>
        val newState = state.removeBuildSubscription(r.sendTo)
        runWith(newState)
        if (canUnwatch(newState, r.sendTo)) context.unwatch(r.sendTo)
        r.unsubscribed()
      case r: WatchTask =>
        runWith(state.addOrUpdateWatchSubscriptions(GenericKey.fromTaskKey(r.key), r.sendTo, () => state.client.rawLazyWatch(r.key)(watchListener(self))))
        context.watch(r.sendTo)
        r.watching(r.key)
      case r: RemoveTaskWatch =>
        val newState = state.removeWatchSubscription(GenericKey.fromTaskKey(r.key), r.sendTo)
        runWith(newState)
        if (canUnwatch(newState, r.sendTo)) context.unwatch(r.sendTo)
        r.removed(r.key)
      case r: WatchSetting =>
        runWith(state.addOrUpdateWatchSubscriptions(GenericKey.fromSettingKey(r.key), r.sendTo, () => state.client.rawLazyWatch(r.key)(watchListener(self))))
        context.watch(r.sendTo)
        r.watching(r.key)
      case r: RemoveSettingWatch =>
        val newState = state.removeWatchSubscription(GenericKey.fromSettingKey(r.key), r.sendTo)
        runWith(newState)
        if (canUnwatch(newState, r.sendTo)) context.unwatch(r.sendTo)
        r.removed(r.key)
    }
  }

  private def running(state: State): Receive = {
    case UpdateClient(c) =>
      val newState = state
        .resubscribeEventSubscriptions(c.handleEvents(eventListener(self)))
        .resubscribeBuildSubscription(c.watchBuild(buildListener(self)))
        .resubscribeWatchSubscriptions(_ match {
          case GenericTaskKey(k, _) => c.rawLazyWatch(k)(watchListener(self))
          case GenericSettingKey(k, _) => c.rawLazyWatch(k)(watchListener(self))
        })
      notificationSink(Notifications.Reconnected)
      runWith(newState)
    case Terminated(t) =>
      context.unwatch(t)
      val newState = state
        .removeEventSubscription(t)
        .removeBuildSubscription(t)
        .removeAllWatchSubscription(t)
      notificationSink(Notifications.HandledTermination)
      runWith(newState)
    case req: LocalRequest[_] => onRequest(req, state, self)
    case pEvent: protocol.Event =>
      state.eventSubscriptions.foreach(_.subscribers.foreach(_ ! pEvent))
    case pEvent: protocol.MinimalBuildStructure =>
      state.buildSubscriptions.foreach(_.subscribers.foreach(_ ! pEvent))
    case event: WatchEvent =>
      state.watchSubscriptions.get(event.key).foreach(_.subscribers.foreach(_ ! event))
  }

  def receive: Receive = running(State(client = initialClient))
}
