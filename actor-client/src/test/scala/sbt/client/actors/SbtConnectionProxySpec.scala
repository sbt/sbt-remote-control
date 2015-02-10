/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import org.junit.{ AfterClass, Assert, BeforeClass, Test, Ignore }
import akka.actor._
import akka.testkit._
import concurrent.ExecutionContext.Implicits.global
import sbt.client.{ Subscription, SbtConnector, SbtClient, Interaction, SettingKey, TaskKey }
import com.typesafe.config.{ ConfigFactory, Config }

object SbtConnectionProxySpec {
  def builderProps(newClient: SbtConnectionProxy.NewClient, notificationSink: ActorRef) = Props(new SbtClientBuilder(newClient, notificationSink))
  def clientProps(client: SbtClient, notificationSink: SbtClientProxy.Notification => Unit = _ => ()) = Props(new SbtClientProxy(client, global, notificationSink))

  class SbtConnectionProxySpecHelper(_system: ActorSystem) extends AkkaTestKitHelper(_system) {
    import SbtConnectionProxy._
    import concurrent.ExecutionContext

    def this(config: Config) = this(ActorSystem(AkkaTestKitHelper.randomActorSystemName, config))
    def this() = this(AkkaTestKitHelper.config)

    def withSbtConnectionProxy[T](conn: SbtConnector,
      client: SbtClient,
      notificationSink: SbtConnectionProxy.Notification => Unit = _ => (),
      testClosed: Boolean = true,
      builderProps: (SbtConnectionProxy.NewClient, ActorRef) => Props = SbtConnectionProxySpec.builderProps _,
      clientProps: SbtClient => Props = SbtConnectionProxySpec.clientProps(_))(body: ActorRef => T)(implicit ex: ExecutionContext): T = {
      import SbtConnectionProxy._
      val cp = system.actorOf(Props(new SbtConnectionProxy(conn, (_ => client), builderProps, clientProps, ex, notificationSink)))
      try {
        body(cp)
      } finally {
        cp ! Close(testActor)
        Assert.assertEquals(expectMsgType[Closed.type], Closed)
        if (testClosed) Assert.assertEquals(client.isClosed, true)
      }
    }
  }

  class SbtConnectionProxySpecification extends Specification[SbtConnectionProxySpecHelper] {
    def gen() = new SbtConnectionProxySpecHelper()
  }
}

class SbtConnectionProxySpec extends SbtConnectionProxySpec.SbtConnectionProxySpecification {
  import SbtConnectionProxySpec._, SbtConnectionProxy._

  @Test
  def testShutdownGracefully(): Unit = withHelper { helper =>
    import helper._
    withFakeEverything() { (conn, client) =>
      withSbtConnectionProxy(conn, client, testClosed = false) { cp => }
    }
  }

  @Test
  def defCreateANewClient(): Unit = withHelper { helper =>
    import helper._
    withFakeEverything() { (conn, client) =>
      withSbtConnectionProxy(conn, client, x => testActor ! x) { cp =>
        cp ! NewClient(testActor)
        expectMsg(Notifications.BuilderAwaitingChannel)
        conn.sendValue(FakeSbtConnector.Channel)
        val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
      }
    }
  }

  @Test
  def testPropogateReconnect(): Unit = withHelper { helper =>
    import helper._
    withFakeEverything() { (conn, client) =>
      withSbtConnectionProxy(conn, client, x => testActor ! x, clientProps = clientProps(_, x => testActor ! x)) { cp =>
        cp ! NewClient(testActor)
        expectMsg(Notifications.BuilderAwaitingChannel)
        conn.sendValue(FakeSbtConnector.Channel)
        val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
        conn.sendValue(FakeSbtConnector.Channel)
        expectMsg(SbtClientProxy.Notifications.Reconnected)
      }
    }
  }

  @Test
  def testAbsorbRecoverableErrors(): Unit = withHelper { helper =>
    import helper._
    withFakeEverything() { (conn, client) =>
      withSbtConnectionProxy(conn, client, x => testActor ! x) { cp =>
        cp ! NewClient(testActor)
        expectMsg(Notifications.BuilderAwaitingChannel)
        conn.sendValue(FakeSbtConnector.Channel)
        val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
        conn.sendValue(FakeSbtConnector.Error(true, "recoverable"))
        expectNoMsg()
      }
    }
  }

  @Test
  def defCleanUpAfterClientClosure(): Unit = withHelper { helper =>
    import helper._
    withFakeEverything() { (conn, client) =>
      withSbtConnectionProxy(conn, client, x => testActor ! x) { cp =>
        cp ! NewClient(testActor)
        expectMsg(Notifications.BuilderAwaitingChannel)
        conn.sendValue(FakeSbtConnector.Channel)
        val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
        proxy ! SbtClientProxy.Close(testActor)
        expectMsgAllClassOf(SbtClientProxy.Closed.getClass(), Notifications.CleanedUpAfterClientClosure.getClass())
      }
    }
  }

  @Test
  def testCleanUpAfterUnrecoverableError(): Unit = withHelper { helper =>
    import helper._
    withFakeEverything() { (conn, client) =>
      withSbtConnectionProxy(conn, client, x => testActor ! x) { cp =>
        cp ! NewClient(testActor)
        expectMsg(Notifications.BuilderAwaitingChannel)
        conn.sendValue(FakeSbtConnector.Channel)
        val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
        conn.sendValue(FakeSbtConnector.Error(false, "unrecoverable"))
        expectMsg(Notifications.ClientClosedDueToDisconnect)
      }
    }
  }
}
