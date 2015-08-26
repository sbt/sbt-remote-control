/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import org.junit.{ AfterClass, Assert, BeforeClass, Test }

import akka.actor._
import akka.testkit._
import concurrent.ExecutionContext.Implicits.global
import sbt.protocol
import java.net.URI
import scala.concurrent.Future
import sbt.client.{ SettingKey, TaskKey }
import sbt.serialization._
import scala.util.{ Try, Success, Failure }

object SbtClientProxySpec {
  val sampleEvent: protocol.Event = protocol.ExecutionStarting(100)
  val sampleBuild: protocol.MinimalBuildStructure = protocol.MinimalBuildStructure(Vector.empty[URI], Vector.empty[protocol.BuildData], Vector.empty[protocol.MinimalProjectStructure])
  val sampleScopedKey = protocol.ScopedKey(protocol.AttributeKey[String]("foo"), protocol.SbtScope())
  val sampleScopedKey1 = protocol.ScopedKey(protocol.AttributeKey[String]("foo1"), protocol.SbtScope())
  def sampleScopedKeyLookup(in: String): Future[Seq[protocol.ScopedKey]] = Future.successful(in match {
    case "foo" => Seq(sampleScopedKey)
    case _ => Seq.empty[protocol.ScopedKey]
  })

  val sampleCompletions = Vector(protocol.Completion("age", "foobage", false), protocol.Completion("bub", "foobub", false))
  val sampleCompletionsFailure = new Exception("Failed to complete")

  def sampleAutocompletions(partialCommand: String, detailLevel: Int): Future[Vector[protocol.Completion]] = partialCommand match {
    case "foo" => Future.successful(sampleCompletions)
    case "bar" => Future.failed(sampleCompletionsFailure)
    case _ => Future.successful(Vector.empty[protocol.Completion])
  }

  val sampleTaskKey = TaskKey[String](sampleScopedKey)
  val sampleSettingKey = SettingKey[String](sampleScopedKey1)
  val sampleBuildValue = protocol.BuildValue("sample")
  val sampleTaskResult = protocol.TaskSuccess(sampleBuildValue)
}

class Forwarder(target: ActorRef) extends Actor with ActorLogging {
  def receive: Receive = {
    case msg => target.tell(msg, sender)
  }
}

class SbtClientProxySpec extends DefaultSpecification {
  import SbtClientProxy._, SbtClientProxySpec._

  @Test
  def testCloseDownGracefully(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      val cp = system.actorOf(Props(new SbtClientProxy(client, global, x => testActor ! x)))
      cp ! Close(testActor)
      Assert.assertEquals(expectMsgType[Closed.type], Closed)
    }
  }

  @Test
  def testSubscribeToEvents(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      withSbtClientProxy(client) { cp =>
        client.sendEvent(sampleEvent)
        expectNoMsg()
        cp ! SubscribeToEvents(testActor)
        Assert.assertEquals(expectMsgType[EventsSubscribed.type], EventsSubscribed)
        client.sendEvent(sampleEvent)
        Assert.assertEquals(expectMsgType[protocol.Event], sampleEvent)
        cp ! UnsubscribeFromEvents(testActor)
        Assert.assertEquals(expectMsgType[EventsUnsubscribed.type], EventsUnsubscribed)
        client.sendEvent(sampleEvent)
        expectNoMsg()
      }
    }
  }

  @Test
  def testSubscribeToBuildEvents(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      withSbtClientProxy(client) { cp =>
        client.sendBuildStructureUpdate(sampleBuild)
        expectNoMsg()
        cp ! SubscribeToBuild(testActor)
        Assert.assertEquals(expectMsgType[BuildSubscribed.type], BuildSubscribed)
        client.sendBuildStructureUpdate(sampleBuild)
        Assert.assertEquals(expectMsgType[protocol.MinimalBuildStructure], sampleBuild)
        cp ! UnsubscribeFromBuild(testActor)
        Assert.assertEquals(expectMsgType[BuildUnsubscribed.type], BuildUnsubscribed)
        client.sendBuildStructureUpdate(sampleBuild)
        expectNoMsg()
      }
    }
  }

  @Test
  def testSubscribeToTaskEvents(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
      withSbtClientProxy(client) { cp =>
        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        expectNoMsg()
        cp ! WatchTask(sampleTaskKey, testActor)
        Assert.assertEquals(expectMsgType[WatchingTask], WatchingTask(sampleTaskKey))
        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey, sampleTaskResult))
        cp ! RemoveTaskWatch(sampleTaskKey, testActor)
        Assert.assertEquals(expectMsgType[TaskWatchRemoved], TaskWatchRemoved(sampleTaskKey))
        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        expectNoMsg()
      }
    }
  }

  @Test
  def testSubscribeToSettingEvents(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
      withSbtClientProxy(client) { cp =>
        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        expectNoMsg()
        cp ! WatchSetting(sampleSettingKey, testActor)
        Assert.assertEquals(expectMsgType[WatchingSetting], WatchingSetting(sampleSettingKey))
        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey1, sampleTaskResult))
        cp ! RemoveSettingWatch(sampleSettingKey, testActor)
        Assert.assertEquals(expectMsgType[SettingWatchRemoved], SettingWatchRemoved(sampleSettingKey))
        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        expectNoMsg()
      }
    }
  }

  @Test
  def testHandleClientTermination(): Unit = withHelper { helper =>
    import helper._
    val forwarder = system.actorOf(Props(new Forwarder(testActor)))
    withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
      withSbtClientProxy(client) { cp =>
        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        client.sendBuildStructureUpdate(sampleBuild)
        client.sendEvent(sampleEvent)
        expectNoMsg()

        cp ! WatchSetting(sampleSettingKey, forwarder)
        Assert.assertEquals(expectMsgType[WatchingSetting], WatchingSetting(sampleSettingKey))
        cp ! WatchTask(sampleTaskKey, forwarder)
        Assert.assertEquals(expectMsgType[WatchingTask], WatchingTask(sampleTaskKey))
        cp ! SubscribeToBuild(forwarder)
        Assert.assertEquals(expectMsgType[BuildSubscribed.type], BuildSubscribed)
        cp ! SubscribeToEvents(forwarder)
        Assert.assertEquals(expectMsgType[EventsSubscribed.type], EventsSubscribed)

        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey, sampleTaskResult))

        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey1, sampleTaskResult))

        client.sendBuildStructureUpdate(sampleBuild)
        Assert.assertEquals(expectMsgType[protocol.MinimalBuildStructure], sampleBuild)

        client.sendEvent(sampleEvent)
        Assert.assertEquals(expectMsgType[protocol.Event], sampleEvent)

        system stop forwarder

        expectMsg(SbtClientProxy.Notifications.HandledTermination)

        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        client.sendBuildStructureUpdate(sampleBuild)
        client.sendEvent(sampleEvent)
        expectNoMsg()
      }
    }
  }

  @Test
  def testProperlyHandleReconnect(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
      withSbtClientProxy(client) { cp =>
        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        client.sendBuildStructureUpdate(sampleBuild)
        client.sendEvent(sampleEvent)
        expectNoMsg()

        cp ! WatchSetting(sampleSettingKey, testActor)
        Assert.assertEquals(expectMsgType[WatchingSetting], WatchingSetting(sampleSettingKey))
        cp ! WatchTask(sampleTaskKey, testActor)
        Assert.assertEquals(expectMsgType[WatchingTask], WatchingTask(sampleTaskKey))
        cp ! SubscribeToBuild(testActor)
        Assert.assertEquals(expectMsgType[BuildSubscribed.type], BuildSubscribed)
        cp ! SubscribeToEvents(testActor)
        Assert.assertEquals(expectMsgType[EventsSubscribed.type], EventsSubscribed)

        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey, sampleTaskResult))

        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey1, sampleTaskResult))

        client.sendBuildStructureUpdate(sampleBuild)
        Assert.assertEquals(expectMsgType[protocol.MinimalBuildStructure], sampleBuild)

        client.sendEvent(sampleEvent)
        Assert.assertEquals(expectMsgType[protocol.Event], sampleEvent)

        cp ! UpdateClient(client)

        expectMsg(SbtClientProxy.Notifications.Reconnected)

        client.sendWatchEvent(sampleScopedKey, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey, sampleTaskResult))

        client.sendWatchEvent(sampleScopedKey1, sampleTaskResult)
        Assert.assertEquals(expectMsgType[WatchEvent], WatchEvent(sampleScopedKey1, sampleTaskResult))

        client.sendBuildStructureUpdate(sampleBuild)
        Assert.assertEquals(expectMsgType[protocol.MinimalBuildStructure], sampleBuild)

        client.sendEvent(sampleEvent)
        Assert.assertEquals(expectMsgType[protocol.Event], sampleEvent)
      }
    }
  }

  @Test
  def testSetDaemon(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient() { client =>
      val cp = system.actorOf(Props(new SbtClientProxy(client, global, x => testActor ! x)))
      Assert.assertFalse("client starts not in daemon mode", client.daemon)
      cp ! SetDaemon(true, testActor)
      Assert.assertEquals(expectMsgType[DaemonSet.type], DaemonSet)
      Assert.assertTrue("client was set to daemon mode", client.daemon)
    }
  }

  @Test
  def testPossibleAutocompletions(): Unit = withHelper { helper =>
    import helper._
    withFakeSbtClient(autocompletions = sampleAutocompletions) { client =>
      withSbtClientProxy(client) { cp =>
        cp ! PossibleAutoCompletions("foo", 0, testActor)
        Assert.assertEquals(expectMsgType[AutoCompletions], AutoCompletions(Success(sampleCompletions)))

        cp ! PossibleAutoCompletions("bar", 0, testActor)
        Assert.assertEquals(expectMsgType[AutoCompletions], AutoCompletions(Failure(sampleCompletionsFailure)))

        cp ! PossibleAutoCompletions("baz", 0, testActor)
        Assert.assertEquals(expectMsgType[AutoCompletions], AutoCompletions(Success(Vector.empty[protocol.Completion])))
      }
    }
  }

}
