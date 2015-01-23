/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import sbt.client.{ Subscription, SbtConnector, SbtClient, Interaction, SettingKey, TaskKey, SbtChannel }
import sbt.client.{ BuildStructureListener, RawValueListener, ValueListener, EventListener }
import sbt.protocol
import sbt.serialization._
import SbtClientProxy.WatchEvent
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean
import akka.actor._
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final class FakeSubscription[T](refFactory: ActorRefFactory, body: T => Unit, ex: ExecutionContext)(implicit mf: Manifest[T]) extends Subscription {
  private final val enabled = new AtomicBoolean(true)

  private final class BodyRunner() extends Actor with ActorLogging {
    def receive: Receive = {
      case mf(x) => if (enabled.get()) {
        ex.prepare.execute(new Runnable() {
          def run(): Unit = {
            body(x)
          }
        })
      }
    }
  }

  final val runner = refFactory.actorOf(Props(new BodyRunner()))

  final def send(in: T): Unit = {
    runner ! in
  }

  final def cancel(): Unit = {
    enabled.set(false)
    refFactory stop runner
  }
}

object WatchSubscriptionManager {
  case class Subscribe(key: protocol.ScopedKey, target: ActorRef)
  case object StopAll
  case object AllStopped
}

class WatchSubscriptionManager(subscriptionManagerProps: Props) extends Actor with ActorLogging {
  import WatchSubscriptionManager._

  private def waitingForTermination(originalSender: ActorRef, awaiting: Set[ActorRef]): Receive = {
    case Terminated(sm) =>
      val newAwaiting = awaiting - sm
      if (!newAwaiting.isEmpty) context.become(waitingForTermination(originalSender, newAwaiting))
      else {
        context stop self
        originalSender ! AllStopped
      }
    case SubscriptionManager.AllStopped => // ignore
  }

  private def run(subscriptions: Map[protocol.ScopedKey, ActorRef]): Receive = {
    case e @ WatchEvent(key, result) =>
      subscriptions.get(key).foreach(_.tell(e, sender))
    case Subscribe(key, target) =>
      val newSubs: Map[protocol.ScopedKey, ActorRef] = subscriptions.get(key) match {
        case Some(sm) =>
          sm ! SubscriptionManager.Subscribe(target)
          subscriptions
        case None =>
          val sm = context.actorOf(subscriptionManagerProps)
          sm ! SubscriptionManager.Subscribe(target)
          subscriptions + (key -> sm)
      }
      context.become(run(newSubs))
    case StopAll =>
      val targets = subscriptions.values.toSet
      targets.foreach(_ ! SubscriptionManager.StopAll)
      context.become(waitingForTermination(sender, targets))
  }

  def receive: Receive = run(Map.empty[protocol.ScopedKey, ActorRef])

}

object SubscriptionManager {
  case class Subscribe(target: ActorRef)
  case object StopAll
  case object AllStopped
}

class SubscriptionManager[T]()(implicit mf: Manifest[T]) extends Actor with ActorLogging {
  import SubscriptionManager._

  private def run(subscriptions: Set[ActorRef]): Receive = {
    case mf(x) => subscriptions.foreach(_.tell(x, sender))
    case Subscribe(target) =>
      context.watch(target)
      context.become(run(subscriptions + target))
    case Terminated(target) =>
      context.unwatch(target)
      context.become(run(subscriptions - target))
    case StopAll =>
      subscriptions.foreach { s =>
        context.unwatch(s)
        context stop s
      }
      context stop self
      sender ! AllStopped
  }

  def receive: Receive = run(Set.empty[ActorRef])
}

object FakeSbtClient {
  def emptyPossibleAutocompletions(partialCommand: String, detailLevel: Int): Future[Vector[protocol.Completion]] =
    Future.successful(Vector.empty[protocol.Completion])

  def emptyLookupScopedKey(name: String): Future[Seq[protocol.ScopedKey]] =
    Future.successful(Seq.empty[protocol.ScopedKey])

  def emptyAnalyzeExecution(command: String): Future[protocol.ExecutionAnalysis] = Future.failed(new Exception("emptyAnalyzeExecution"))

  def emptyRequestExecutionCommand(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] =
    Future.successful(-1L)

  def emptyRequestExecutionKey(key: protocol.ScopedKey, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] =
    Future.successful(-1L)

  def emptyCancelExecution(id: Long): Future[Boolean] = Future.successful(true)

}

final class FakeSbtClient(refFactory: ActorRefFactory,
  val configName: String,
  val humanReadableName: String,
  val channel: SbtChannel = null,
  autocompletions: (String, Int) => Future[Vector[protocol.Completion]] = FakeSbtClient.emptyPossibleAutocompletions,
  scopedKeyLookup: String => Future[Seq[protocol.ScopedKey]] = FakeSbtClient.emptyLookupScopedKey,
  _analyzeExecution: String => Future[protocol.ExecutionAnalysis] = FakeSbtClient.emptyAnalyzeExecution,
  requestExecutionCommand: (String, Option[(Interaction, ExecutionContext)]) => Future[Long] = FakeSbtClient.emptyRequestExecutionCommand,
  requestExecutionKey: (protocol.ScopedKey, Option[(Interaction, ExecutionContext)]) => Future[Long] = FakeSbtClient.emptyRequestExecutionKey,
  _cancelExecution: Long => Future[Boolean] = FakeSbtClient.emptyCancelExecution,
  val uuid: java.util.UUID = UUID.randomUUID()) extends SbtClient {
  private final val closed = new AtomicBoolean(false)
  private final val buildStructureQueue = refFactory.actorOf(Props(new SubscriptionManager[protocol.MinimalBuildStructure]()))
  private final val eventQueue = refFactory.actorOf(Props(new SubscriptionManager[protocol.Event]()))
  private final val watchQueue = refFactory.actorOf(Props(new WatchSubscriptionManager(Props(new SubscriptionManager[WatchEvent]()))))

  def sendBuildStructureUpdate(in: protocol.MinimalBuildStructure): Unit = buildStructureQueue ! in
  def sendEvent(in: protocol.Event): Unit = eventQueue ! in
  def sendWatchEvent(key: protocol.ScopedKey, result: protocol.TaskResult): Unit = watchQueue ! WatchEvent(key, result)

  def watchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription = {
    val r = new FakeSubscription[protocol.MinimalBuildStructure](refFactory, listener, ex)
    buildStructureQueue ! SubscriptionManager.Subscribe(r.runner)
    r
  }

  def lazyWatchBuild(listener: BuildStructureListener)(implicit ex: ExecutionContext): Subscription =
    watchBuild(listener)(ex)

  def possibleAutocompletions(partialCommand: String, detailLevel: Int): Future[Vector[protocol.Completion]] =
    autocompletions(partialCommand, detailLevel)

  def lookupScopedKey(name: String): Future[Seq[protocol.ScopedKey]] = scopedKeyLookup(name)

  def analyzeExecution(command: String): Future[protocol.ExecutionAnalysis] = _analyzeExecution(command)

  def requestExecution(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] =
    requestExecutionCommand(commandOrTask, interaction)

  def requestExecution(key: protocol.ScopedKey, interaction: Option[(Interaction, ExecutionContext)]): Future[Long] =
    requestExecutionKey(key, interaction)

  def cancelExecution(id: Long): Future[Boolean] = _cancelExecution(id)

  def handleEvents(listener: EventListener)(implicit ex: ExecutionContext): Subscription = {
    val r = new FakeSubscription[protocol.Event](refFactory, listener, ex)
    eventQueue ! SubscriptionManager.Subscribe(r.runner)
    r
  }

  def rawWatch(key: SettingKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription = {
    def unwrapEvent(e: WatchEvent): Unit = listener(e.key, e.result)
    val r = new FakeSubscription[WatchEvent](refFactory, unwrapEvent, ex)
    watchQueue ! WatchSubscriptionManager.Subscribe(key.key, r.runner)
    r
  }

  def rawLazyWatch(key: SettingKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    rawWatch(key)(listener)(ex)

  def rawWatch(key: TaskKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription = {
    def unwrapEvent(e: WatchEvent): Unit = {
      listener(e.key, e.result)
    }
    val r = new FakeSubscription[WatchEvent](refFactory, unwrapEvent, ex)
    watchQueue ! WatchSubscriptionManager.Subscribe(key.key, r.runner)
    r
  }

  def rawLazyWatch(key: TaskKey[_])(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    rawWatch(key)(listener)(ex)

  def watch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit unpickle: Unpickler[T], ex: ExecutionContext): Subscription =
    rawWatch(key) {
      case (k, v) =>
        listener(k, v.result[T])
    }

  def lazyWatch[T](key: SettingKey[T])(listener: ValueListener[T])(implicit unpickle: Unpickler[T], ex: ExecutionContext): Subscription =
    watch[T](key)(listener)(unpickle, ex)

  def watch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit unpickle: Unpickler[T], ex: ExecutionContext): Subscription =
    rawWatch(key) {
      case (k, v) =>
        listener(k, v.result[T])
    }

  def lazyWatch[T](key: TaskKey[T])(listener: ValueListener[T])(implicit unpickle: Unpickler[T], ex: ExecutionContext): Subscription =
    watch[T](key)(listener)(unpickle, ex)

  def watch[T](name: String)(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    ??? // TODO stubbed until someone actually wants to use it

  def lazyWatch[T](name: String)(listener: ValueListener[T])(implicit unpickler: Unpickler[T], ex: ExecutionContext): Subscription =
    ??? // TODO stubbed until someone actually wants to use it

  def rawWatch(name: String)(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    ??? // TODO stubbed until someone actually wants to use it

  def rawLazyWatch(name: String)(listener: RawValueListener)(implicit ex: ExecutionContext): Subscription =
    ??? // TODO stubbed until someone actually wants to use it

  def requestSelfDestruct(): Unit = ()
  def isClosed: Boolean = closed.get()

  def close(): Unit = {
    import refFactory.dispatcher
    // TODO - Configure this separately for travis.
    implicit val timeout = Timeout(10.seconds)
    closed.set(true)
    for {
      _ <- ask(buildStructureQueue, SubscriptionManager.StopAll)
      _ <- ask(eventQueue, SubscriptionManager.StopAll)
      _ <- ask(watchQueue, WatchSubscriptionManager.StopAll)
    } {}
  }
}
