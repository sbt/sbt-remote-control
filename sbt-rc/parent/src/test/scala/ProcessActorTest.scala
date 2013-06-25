/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
import java.io.File
import akka.actor._
import akka.dispatch._
import akka.util._
import scala.concurrent._
import scala.concurrent.duration._

class TestRecorder(endPredicate: Any => Boolean, resultPromise: Promise[Seq[Any]]) extends Actor {
  var record: Vector[Any] = Vector.empty
  override def receive = {
    case m =>
      record = record :+ m
      if (endPredicate(m)) {
        resultPromise.success(record)
        context.stop(self)
      }
  }
}

class ProcessActorTest {

  private def recordProcess(systemName: String, argv: Seq[String], cwd: File): Seq[Any] = {
    val system = ActorSystem(systemName)
    val process = ProcessActor(system, "testProcess", argv, cwd)
    val resultPromise = Promise[Seq[Any]]()
    val recorder = system.actorOf(Props(new TestRecorder({
      case stopped: ProcessStopped => true
      case _ => false
    }, resultPromise)), name = "recorder")
    process ! SubscribeProcess(recorder)
    process ! StartProcess

    val result = Await.result(resultPromise.future, 3.seconds)

    system.shutdown()
    system.awaitTermination()

    result
  }

  case class Output(stdout: String, stderr: String, events: Seq[ProcessEvent])

  private def extractOutput(recorded: Seq[Any]): Output = {
    val out = new ByteStringDecoder
    val err = new ByteStringDecoder
    val events = Seq.newBuilder[ProcessEvent]
    recorded foreach {
      case ProcessStdOut(bytes) =>
        out.feed(bytes)
      case ProcessStdErr(bytes) =>
        err.feed(bytes)
      case event: ProcessEvent =>
        events += event
    }

    out.finish()
    err.finish()

    Output(out.read.mkString, err.read.mkString, events.result)
  }

  @Test
  def testSimpleProcess(): Unit = {
    val output = extractOutput(recordProcess("testSimpleProcess", Seq("echo", "hello world"), new File(".")))
    assertEquals("hello world\n", output.stdout)
    assertEquals("", output.stderr)
    assertEquals(Seq(ProcessStopped(0)), output.events)
  }
}
