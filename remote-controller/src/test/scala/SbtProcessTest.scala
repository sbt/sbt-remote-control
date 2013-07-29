/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
import com.typesafe.sbtrc.protocol._
import java.io.File
import akka.actor._
import akka.dispatch._
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern._
import akka.util.Timeout

class SbtProcessTest {

  // this is ANNOYINGLY long if you're debugging tests by hand, but
  // we don't want it to keep randomly failing.
  // just edit it shorter locally if you are hitting it when coding.
  implicit val timeout = Timeout(300.seconds)

  // TODO - Get the scratch dir form somewhere else useful, like an environment variable,
  // so it's directly tied to project layout.
  val testUtil = new TestUtil(scratchDir = new File("sbt-rc/parent/target/scratch"))

  import testUtil._

  private class TestRequestActor(dummy: File) extends Actor with ActorLogging {
    val child = SbtProcess(context, dummy, DebugSbtProcessLauncher)

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

  private def requestTest(dummy: File)(sendRequest: (ActorRef, ActorContext) => Unit)(checkResults: Seq[protocol.Message] => Unit): Unit = {
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

  private def noLogs(results: Seq[protocol.Message]): Seq[protocol.Message] = {
    results flatMap {
      case e: protocol.LogEvent => Seq.empty
      case m: protocol.Message => Seq(m)
    }
  }

  // this is just an ad hoc attempt to make the event order more
  // deterministic. We assume a stable sort.
  private implicit val messageOrdering: Ordering[protocol.Message] = new Ordering[protocol.Message]() {
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

  @Test
  def testRunTests(): Unit = {
    requestTest(makeDummySbtProject("testing123")) { (child, context) =>
      implicit val self = context.self
      child ! TestRequest(sendEvents = true)
    } { results =>
      noLogs(results).sorted match {
        case Seq(Started,
          RequestReceivedEvent,
          TestEvent("OneFailTest.testThatShouldFail",
            Some("this is not true"), TestFailed, Some("this is not true")),
          TestEvent("OnePassOneFailTest.testThatShouldFail",
            Some("this is not true"), TestFailed, Some("this is not true")),
          TestEvent("OnePassOneFailTest.testThatShouldPass", None, TestPassed, None),
          TestEvent("OnePassTest.testThatShouldPass", None, TestPassed, None),
          ErrorResponse("exception during sbt task: test: Incomplete: null")) =>
        // yay!
        case whatever => throw new AssertionError("got wrong results: " + whatever)
      }
    }
  }

  @Test
  def testTalkToChild(): Unit = {
    val dummy = makeDummySbtProject("talkToChild")

    val system = ActorSystem("test-talk-to-child")
    try {
      val child = SbtProcess(system, dummy, DebugSbtProcessLauncher)

      try {
        val name = Await.result(child ? NameRequest(sendEvents = false), timeout.duration) match {
          case NameResponse(n) => n
        }
        assertEquals("talkToChild", name)

        val name2 = Await.result(child ? NameRequest(sendEvents = false), timeout.duration) match {
          case NameResponse(n) => n
        }
        assertEquals("talkToChild", name2)

      } finally {
        system.stop(child)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

  object EchoHelloChildProcessMaker extends SbtProcessLauncher {

    private def isWindows =
      sys.props("os.name").toLowerCase contains "windows"

    override def apply(cwd: File, port: Int): ProcessBuilder = {
      val cmd =
        if (isWindows) Seq("cmd.exe", "/c", "echo", "Hello World")
        else Seq("echo", "Hello World")
      import collection.JavaConverters._
      (new ProcessBuilder).command(cmd.asJava).directory(cwd)
    }
  }

  @Test
  def testChildProcessNeverConnects(): Unit = {
    val system = ActorSystem("test-child-never-connects")

    val child = SbtProcess(system, new File("."), EchoHelloChildProcessMaker)

    // the test is that the child should die on its own so actor system shutdown should work

    system.shutdown()
    system.awaitTermination()
  }

  @Test
  def testRunChild(): Unit = {
    val dummy = makeDummySbtProject("runChild")

    val system = ActorSystem("test-run-child")
    try {
      val child = SbtProcess(system, dummy, DebugSbtProcessLauncher)

      try {
        Await.result(child ? RunRequest(sendEvents = false, mainClass = None), timeout.duration) match {
          case RunResponse(success, "run") =>
          case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
        }
      } finally {
        system.stop(child)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

  @Test
  def testBrokenBuild(): Unit = {
    val dummy = makeDummySbtProjectWithBrokenBuild("brokenBuild")

    val system = ActorSystem("test-broken-build")
    try {
      val child = SbtProcess(system, dummy, DebugSbtProcessLauncher)

      try {
        Await.result(child ? RunRequest(sendEvents = false, mainClass = None), timeout.duration) match {
          case ErrorResponse(message) if message.contains("sbt process never got in touch") =>
          case whatever => throw new AssertionError("unexpected result sending RunRequest to broken build: " + whatever)
        }
      } finally {
        system.stop(child)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

  @Test
  def testRunWithMissingMain(): Unit = {
    val dummy = makeDummySbtProjectWithNoMain("noMainRun")

    val system = ActorSystem("test-no-main-run")
    try {
      val child = SbtProcess(system, dummy, DebugSbtProcessLauncher)

      try {
        Await.result(child ? RunRequest(sendEvents = false, mainClass = None), timeout.duration) match {
          case ErrorResponse(message) if message.contains("during sbt task: run: Incomplete") =>
          case whatever => throw new AssertionError("unexpected result sending RunRequest to app with no main method: " + whatever)
        }
      } finally {
        system.stop(child)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

  @Test
  def testDiscoverMissingMain(): Unit = {
    val dummy = makeDummySbtProjectWithNoMain("noMainDiscover")

    val system = ActorSystem("test-no-main-discover")
    try {
      val child = SbtProcess(system, dummy, DebugSbtProcessLauncher)

      try {
        Await.result(child ? DiscoveredMainClassesRequest(sendEvents = false), timeout.duration) match {
          case DiscoveredMainClassesResponse(Seq()) =>
          case whatever => throw new AssertionError("unexpected result sending DiscoveredMainClassesRequest to app with no main method: " + whatever)
        }
      } finally {
        system.stop(child)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

  @Test
  def testDiscoverMultipleMain(): Unit = {
    val dummy = makeDummySbtProjectWithMultipleMain("multiMainDiscover")

    val system = ActorSystem("test-multi-main-discover")
    try {
      val child = SbtProcess(system, dummy, DebugSbtProcessLauncher)

      try {
        Await.result(child ? DiscoveredMainClassesRequest(sendEvents = false), timeout.duration) match {
          case DiscoveredMainClassesResponse(Seq("Main1", "Main2", "Main3")) =>
          case whatever => throw new AssertionError("unexpected result sending DiscoveredMainClassesRequest to app with multi main method: " + whatever)
        }
      } finally {
        system.stop(child)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

  @Test
  def testRunMultipleMain(): Unit = {
    val dummy = makeDummySbtProjectWithMultipleMain("runSelectingAMain")

    val system = ActorSystem("test-run-selecting-a-main")
    try {
      val child = SbtProcess(system, dummy, DebugSbtProcessLauncher)

      try {
        Await.result(child ? RunRequest(sendEvents = false, mainClass = Some("Main2")), timeout.duration) match {
          case RunResponse(success, "run-main") =>
          case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
        }
        Await.result(child ? RunRequest(sendEvents = false, mainClass = Some("Main3")), timeout.duration) match {
          case RunResponse(success, "run-main") =>
          case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
        }
      } finally {
        system.stop(child)
      }

    } finally {
      system.shutdown()
      system.awaitTermination()
    }
  }

}
