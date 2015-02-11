/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import org.junit.{ AfterClass, Assert, BeforeClass, Test }
import akka.actor._
import akka.testkit._
import concurrent.ExecutionContext.Implicits.global
import sbt.client.{ Subscription, SbtConnector, SbtClient, Interaction, SettingKey, TaskKey }

object SbtClientBuilderSpec {
  val fakeSubscription: Subscription = new Subscription {
    def cancel(): Unit = {}
  }

  val reconnectableErrorResult = SbtClientBuilder.Error(true, "reconnectable")
  val notReconnectableErrorResult = SbtClientBuilder.Error(false, "not reconnectable")

  class Forwarder(sink: ActorRef) extends Actor {
    val receive: Receive = {
      case m: SbtClientProxy.Close =>
        sink ! m
        m.closed()
        context stop self
      case m => sink ! m
    }
  }

  def forwrderProps(sink: ActorRef): Props = Props(new Forwarder(sink))
}

class SbtClientBuilderSpec extends DefaultSpecification {
  import SbtClientBuilderSpec._, SbtClientBuilder._

  @Test
  def testNewClientConnection(): Unit = withHelper { helper =>
    import helper._

    def fakeClientBuilder(client: SbtClient): Props = forwrderProps(testActor)

    withFakeSbtClient() { client =>
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, fakeClientBuilder)))
      cb ! Subscription(fakeSubscription)
      cb ! Client(client)
      expectMsgType[SbtConnectionProxy.NewClientResponse.Connected]
      system stop cb
    }
  }

  @Test
  def testDeliverAnError(): Unit = withHelper { helper =>
    import helper._

    def fakeClientBuilder(client: SbtClient): Props = forwrderProps(testActor)

    val probe = TestProbe()
    val nc = SbtConnectionProxy.NewClient(testActor)
    val cb = system.actorOf(Props(new SbtClientBuilder(nc, fakeClientBuilder)))
    probe.watch(cb)
    cb ! Subscription(fakeSubscription)
    cb ! reconnectableErrorResult
    expectNoMsg()
    cb ! notReconnectableErrorResult
    expectMsgType[SbtConnectionProxy.NewClientResponse.Error]
    probe.expectTerminated(cb)
  }

  @Test
  def testHandleErrorsAfterConntect(): Unit = withHelper { helper =>
    import helper._

    def fakeClientBuilder(client: SbtClient): Props = forwrderProps(testActor)

    withFakeSbtClient() { client =>
      val probe = TestProbe()
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, fakeClientBuilder)))
      probe.watch(cb)
      cb ! Subscription(fakeSubscription)
      cb ! Client(client)
      expectMsgType[SbtConnectionProxy.NewClientResponse.Connected]
      cb ! reconnectableErrorResult
      expectNoMsg()
      cb ! notReconnectableErrorResult
      expectMsgType[SbtClientProxy.Close]
      probe.expectTerminated(cb)
    }
  }

  @Test
  def testHandleReconnect(): Unit = withHelper { helper =>
    import helper._

    def fakeClientBuilder(client: SbtClient): Props = forwrderProps(testActor)

    withFakeSbtClient() { client =>
      val probe = TestProbe()
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, fakeClientBuilder)))
      probe.watch(cb)
      cb ! Subscription(fakeSubscription)
      cb ! Client(client)
      expectMsgType[SbtConnectionProxy.NewClientResponse.Connected]
      cb ! Client(client)
      expectMsgType[SbtClientProxy.UpdateClient]
      cb ! reconnectableErrorResult
      expectNoMsg()
      cb ! Client(client)
      expectMsgType[SbtClientProxy.UpdateClient]
      cb ! notReconnectableErrorResult
      expectMsgType[SbtClientProxy.Close]
      probe.expectTerminated(cb)
    }
  }
}
