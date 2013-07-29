package com.typesafe.sbtrc
package it

import java.io.File
import xsbti.Repository
import xsbti.MavenRepository
import xsbti.IvyRepository

trait SbtProcessLauncherTest extends IntegrationTest {
  /** The local repositories we need to use for this integration test. */
  def localRepositories: Seq[(String, File, Option[String])] = {
    def getFileLocationAndName(repo: Repository): Option[(String, File, Option[String])] =
      repo match {
        // TODO - Support maven too?
        case x: IvyRepository if x.url.getProtocol == "file" =>
          Some(RepoHelper.ivy(x.id, new File(x.url.toURI)))
        case x: MavenRepository if x.url.getProtocol == "file" =>
          Some(RepoHelper.mvn(x.id, new File(x.url.toURI)))
        case _ => None
      }

    for {
      repo <- repositories
      values <- getFileLocationAndName(repo)
    } yield values
  }
  /** Constructs a new sbt process launcher using the repositories from our own launched app. */
  def sbtProcessLauncher: SbtProcessLauncher =
    new DefaultSbtProcessLauncher(configuration,
      // TODO - Figure out a way to prevent reading the user's ~/.sbt/repositories file in favor
      // of our locally defined repositories....
      optionalRepositories = localRepositories)

  lazy val utils = new TestUtil(new File("scratch"))

  import akka.actor._
  import concurrent.Await
  import concurrent.duration._
  import akka.util.Timeout
  import akka.pattern.ask
  implicit lazy val timeout = Timeout(300.seconds)

  private class TestRequestActor(dummy: File) extends Actor with ActorLogging {
    val child = SbtProcess(context, dummy, sbtProcessLauncher)

    var recorded: Seq[protocol.Message] = Nil
    var requestor: Option[ActorRef] = None
    var done = false

    override def receive = {
      case m: protocol.Message =>
        recorded = recorded :+ m
        m match {
          case r: protocol.Response =>
            done = true
            requestor.foreach(_ ! recorded)
          case _ =>
        }
      case "get" =>
        if (done)
          sender ! recorded
        else
          requestor = Some(sender)
    }
  }
  protected def requestTest(dummy: File)(sendRequest: (ActorRef, ActorContext) => Unit)(checkResults: Seq[protocol.Message] => Unit): Unit = {
    val system = ActorSystem("test-" + dummy.getName)
    try {
      val req = system.actorOf(Props(new TestRequestActor(dummy) {
        sendRequest(child, context)
      }))

      Await.result(req ? "get", timeout.duration) match {
        case s: Seq[_] =>
          checkResults(s.collect({ case m: protocol.Message => m }))
        case whatever => throw new AssertionError("unexpected reply from TestRequestActor: " + whatever)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

  protected def noLogs(results: Seq[protocol.Message]): Seq[protocol.Message] = {
    results flatMap {
      case e: protocol.LogEvent => Seq.empty
      case m: protocol.Message => Seq(m)
    }
  }

  // this is just an ad hoc attempt to make the event order more
  // deterministic. We assume a stable sort.
  protected implicit lazy val messageOrdering: Ordering[protocol.Message] = new Ordering[protocol.Message]() {
    override def compare(a: protocol.Message, b: protocol.Message): Int = {
      (a, b) match {
        // sort test events by the test name since they
        // otherwise arrive in undefined order
        case (a: protocol.TestEvent, b: protocol.TestEvent) =>
          a.name.compareTo(b.name)
        // leave it alone
        case (a, b) =>
          0
      }
    }
  }
}