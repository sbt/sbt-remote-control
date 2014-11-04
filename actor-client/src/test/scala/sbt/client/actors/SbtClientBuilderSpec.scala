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
}

class SbtClientBuilderSpec extends DefaultSpecification {
  import SbtClientBuilderSpec._, SbtClientBuilder._

  @Test
  def testNewClientConnection(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
      cb ! Subscription(fakeSubscription)
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Result.Success], Result.Success(nc, client, fakeSubscription, cb))
      system stop cb
    }
  }

  @Test
  def testDeliverAnError(): Unit = withHelper { helper =>
    import helper._
    val nc = SbtConnectionProxy.NewClient(testActor)
    val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
    cb ! Subscription(fakeSubscription)
    cb ! reconnectableErrorResult
    expectNoMsg()
    cb ! notReconnectableErrorResult
    Assert.assertEquals(expectMsgType[Result.Failure], Result.Failure(nc, fakeSubscription, notReconnectableErrorResult))
  }

  @Test
  def testHandleErrorsAfterConntect(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
      cb ! Subscription(fakeSubscription)
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Result.Success], Result.Success(nc, client, fakeSubscription, cb))
      cb ! reconnectableErrorResult
      expectNoMsg()
      cb ! notReconnectableErrorResult
      Assert.assertEquals(expectMsgType[Disconnect], Disconnect(fakeSubscription, cb))
    }
  }

  @Test
  def testHandleReconnect(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
      cb ! Subscription(fakeSubscription)
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Result.Success], Result.Success(nc, client, fakeSubscription, cb))
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Reconnect], Reconnect(fakeSubscription, client, cb))
      cb ! reconnectableErrorResult
      expectNoMsg()
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Reconnect], Reconnect(fakeSubscription, client, cb))
      cb ! notReconnectableErrorResult
      Assert.assertEquals(expectMsgType[Disconnect], Disconnect(fakeSubscription, cb))
    }
  }

  @Test
  def testNotDeliverAnythingAfterDisconnect(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
      cb ! Subscription(fakeSubscription)
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Result.Success], Result.Success(nc, client, fakeSubscription, cb))
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Reconnect], Reconnect(fakeSubscription, client, cb))
      cb ! reconnectableErrorResult
      expectNoMsg()
      cb ! Client(client)
      Assert.assertEquals(expectMsgType[Reconnect], Reconnect(fakeSubscription, client, cb))
      cb ! notReconnectableErrorResult
      Assert.assertEquals(expectMsgType[Disconnect], Disconnect(fakeSubscription, cb))
      cb ! reconnectableErrorResult
      expectNoMsg()
      cb ! Client(client)
      expectNoMsg()
      cb ! notReconnectableErrorResult
      expectNoMsg()
    }
  }
}
