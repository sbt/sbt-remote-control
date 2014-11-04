/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import sbt.client.{ Subscription, SbtConnector, SbtClient, Interaction, SettingKey, TaskKey, SbtChannel }
import sbt.client.{ BuildStructureListener, RawValueListener, ValueListener, EventListener }
import scala.concurrent.ExecutionContext
import akka.actor._
import akka.util.Timeout
import akka.pattern.ask
import SbtClientProxy.WatchEvent
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._

object FakeSbtConnector {
  sealed trait Value
  case class Client(client: FakeSbtClient) extends Value
  case class Error(restart: Boolean, message: String) extends Value
  case object Channel extends Value
}

final class FakeSbtConnector(refFactory: ActorRefFactory) extends SbtConnector {
  import FakeSbtConnector._
  private val connections = refFactory.actorOf(Props(new SubscriptionManager[Value]()))

  def sendValue(in: Value): Unit = connections ! in

  def open(onConnect: SbtClient => Unit, onError: (Boolean, String) => Unit)(implicit ex: ExecutionContext): Subscription = {
    def unwrapEvent(e: Value): Unit = e match {
      case Client(c) => onConnect(c)
      case Error(restart, message) => onError(restart, message)
      case Channel => // ignore
    }
    val r = new FakeSubscription[Value](refFactory, unwrapEvent, ex)
    connections ! SubscriptionManager.Subscribe(r.runner)
    r
  }
  def openChannel(onConnect: SbtChannel => Unit, onError: (Boolean, String) => Unit)(implicit ex: ExecutionContext): Subscription = {
    def unwrapEvent(e: Value): Unit = e match {
      case Client(_) => // ignore
      case Error(restart, message) => onError(restart, message)
      case Channel => onConnect(FakeSbtChannel.empty)
    }
    val r = new FakeSubscription[Value](refFactory, unwrapEvent, ex)
    connections ! SubscriptionManager.Subscribe(r.runner)
    r
  }

  def close(): Unit = {
    import refFactory.dispatcher
    implicit val timeout = Timeout(5.seconds)
    for {
      _ <- ask(connections, SubscriptionManager.StopAll)
    } {}
  }

}
