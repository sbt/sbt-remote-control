/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package sbt.client.actors

import org.junit.{ AfterClass, Assert, BeforeClass, Test }
import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._
import akka.testkit._
import com.typesafe.config.{ ConfigFactory, Config }
import scala.util.Random
import sbt.client.{ Subscription, SbtConnector, SbtClient, Interaction, SettingKey, TaskKey, SbtChannel }
import scala.concurrent.ExecutionContext
import sbt.protocol
import scala.concurrent.Future

object AkkaTestKitHelper {
  val configString =
    """
      |akka {
      |  loglevel = "OFF"
      |  stdout-loglevel = "OFF"
      |}
    """.stripMargin

  val config = ConfigFactory.parseString(configString)
  def randomActorSystemName: String = s"test-actor-system-${new String(Random.alphanumeric.take(10).toArray)}"
}

class AkkaTestKitHelper(_system: ActorSystem) extends TestKit(_system) with ImplicitSender {
  def this(config: Config) = this(ActorSystem(AkkaTestKitHelper.randomActorSystemName, config))
  def this() = this(AkkaTestKitHelper.config)

  def after() = system.shutdown()

  lazy val name: String = s"test-${new String(Random.alphanumeric.take(10).toArray)}"

  def withFakeSbtConnector[T](body: FakeSbtConnector => T): T = {
    val connector = new FakeSbtConnector(system)
    try {
      body(connector)
    } finally {
      connector.close()
    }
  }

  def withFakeSbtClient[T](configName: String = name,
    humanReadableName: String = name,
    autocompletions: (String, Int) => Future[Vector[protocol.Completion]] = FakeSbtClient.emptyPossibleAutocompletions,
    scopedKeyLookup: String => Future[Seq[protocol.ScopedKey]] = FakeSbtClient.emptyLookupScopedKey,
    analyzeExecution: String => Future[protocol.ExecutionAnalysis] = FakeSbtClient.emptyAnalyzeExecution,
    requestExecutionCommand: (String, Option[(Interaction, ExecutionContext)]) => Future[Long] = FakeSbtClient.emptyRequestExecutionCommand,
    requestExecutionKey: (protocol.ScopedKey, Option[(Interaction, ExecutionContext)]) => Future[Long] = FakeSbtClient.emptyRequestExecutionKey,
    cancelExecution: Long => Future[Boolean] = FakeSbtClient.emptyCancelExecution)(body: FakeSbtClient => T): T = {
    val client = new FakeSbtClient(refFactory = system,
      configName = configName,
      humanReadableName = humanReadableName,
      autocompletions = autocompletions,
      scopedKeyLookup = scopedKeyLookup,
      _analyzeExecution = analyzeExecution,
      requestExecutionCommand = requestExecutionCommand,
      requestExecutionKey = requestExecutionKey,
      _cancelExecution = cancelExecution)
    try {
      body(client)
    } finally {
      client.close()
    }
  }

  def withSbtClientProxy[T](client: SbtClient)(body: ActorRef => T)(implicit ex: ExecutionContext): T = {
    import SbtClientProxy._
    val cp = system.actorOf(Props(new SbtClientProxy(client, ex, x => testActor ! x)))
    try {
      body(cp)
    } finally {
      cp ! Close(testActor)
      Assert.assertEquals(expectMsgType[Closed.type], Closed)
    }
  }

  def withFakeEverything[T](configName: String = name,
    humanReadableName: String = name,
    autocompletions: (String, Int) => Future[Vector[protocol.Completion]] = FakeSbtClient.emptyPossibleAutocompletions,
    scopedKeyLookup: String => Future[Seq[protocol.ScopedKey]] = FakeSbtClient.emptyLookupScopedKey,
    analyzeExecution: String => Future[protocol.ExecutionAnalysis] = FakeSbtClient.emptyAnalyzeExecution,
    requestExecutionCommand: (String, Option[(Interaction, ExecutionContext)]) => Future[Long] = FakeSbtClient.emptyRequestExecutionCommand,
    requestExecutionKey: (protocol.ScopedKey, Option[(Interaction, ExecutionContext)]) => Future[Long] = FakeSbtClient.emptyRequestExecutionKey,
    cancelExecution: Long => Future[Boolean] = FakeSbtClient.emptyCancelExecution)(body: (FakeSbtConnector, FakeSbtClient) => T): T =
    withFakeSbtConnector { connector =>
      withFakeSbtClient(configName = configName,
        humanReadableName = humanReadableName,
        autocompletions = autocompletions,
        scopedKeyLookup = scopedKeyLookup,
        analyzeExecution = analyzeExecution,
        requestExecutionCommand = requestExecutionCommand,
        requestExecutionKey = requestExecutionKey,
        cancelExecution = cancelExecution) { client =>
          body(connector, client)
        }
    }
}

abstract class Specification[T <: AkkaTestKitHelper] {
  def gen(): T
  def withHelper[U](body: T => U): U = {
    val h = gen()
    try body(h)
    finally (h.after())
  }
}

class DefaultSpecification extends Specification[AkkaTestKitHelper] {
  def gen() = new AkkaTestKitHelper()
}
