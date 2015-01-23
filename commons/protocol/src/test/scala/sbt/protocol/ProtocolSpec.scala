package sbt.protocol.spec

import sbt.protocol
import sbt.protocol.{ Message, CoreProtocol }
import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import SpecsUtil._
import sbt.serialization.spec.JUnitUtil._
import JUnitMessageUtil._
import xsbti.Severity.{ Info, Warn, Error }
import scala.util.{ Try, Success, Failure }
import sbt.serialization.json._
import scala.pickling.{ PickleOps, SPickler, Unpickler }
import sbt.serialization._
import scala.pickling.Defaults.pickleOps

class ProtocolTest {
  import CoreProtocol._

  val key = protocol.AttributeKey("name", TypeExpression.parse("java.lang.String")._1)
  val build = new java.net.URI("file:///test/project")
  val projectRef = protocol.ProjectReference(build, "test")
  val scope = protocol.SbtScope(project = Some(projectRef))
  val scopedKey = protocol.ScopedKey(key, scope)
  val buildStructure = protocol.MinimalBuildStructure(
    builds = Vector(build),
    projects = Vector(protocol.MinimalProjectStructure(scope.project.get, Vector("com.foo.Plugin"))))
  val nopos = protocol.Position(None, None, None, "", None, None, None)
  val severity = Error

  @Test
  def testMessages: Unit = {
    // messages
    roundTripMessage(protocol.KillServerRequest())
    roundTripMessage(protocol.ReadLineRequest(42, "HI", true))
    roundTripMessage(protocol.ReadLineResponse(Some("line")))
    roundTripMessage(protocol.ConfirmRequest(43, "msg"))
    roundTripMessage(protocol.ReadLineResponse(Some("line")))
    roundTripMessage(protocol.ReceivedResponse())
    roundTripMessage(protocol.CommandCompletionsRequest("He", 2))
    roundTripMessage(protocol.CommandCompletionsResponse(Vector(protocol.Completion("llo", "Hello", true))))
    roundTripMessage(protocol.ListenToEvents())
    roundTripMessage(protocol.ListenToBuildChange())
    roundTripMessage(protocol.ExecutionRequest("test command string"))
    roundTripMessage(protocol.ListenToValue(scopedKey))
    roundTripMessage(protocol.CancelExecutionRequest(1))
    roundTripMessage(protocol.ErrorResponse("ZOMG"))
    roundTripMessage(protocol.CancelExecutionResponse(false))
  }

  @Test
  def testEvents: Unit = {
    // events
    roundTripMessage(protocol.TaskStarted(47, 1, Some(scopedKey)))
    roundTripMessage(protocol.TaskFinished(48, 1, Some(scopedKey), true, Some("this is a message")))
    roundTripMessage(protocol.TaskStarted(47, 1, None))
    roundTripMessage(protocol.TaskFinished(48, 1, None, true, None))
    roundTripMessage(protocol.BuildStructureChanged(buildStructure))
  }

  @Test
  def testValueChanged: Unit = {
    val taskSuccess = protocol.TaskSuccess(protocol.BuildValue("HI"))
    val v1: Message = protocol.ValueChanged(scopedKey, taskSuccess)
    val recovered1 = v1.pickle.value.unpickle[protocol.Message] match {
      case v: protocol.ValueChanged => v
      case other => throw new AssertionError(s"expecting ValueChanged got $other")
    }
    recovered1.value.result[String] match {
      case Success("HI") => ()
      case _ => sys.error("unexpected failure")
    }
    roundTripMessage(v1)

    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f))))
    val taskFailure = protocol.TaskFailure(protocol.BuildValue(new Exception("bam"): Throwable))
    val v2: Message = protocol.ValueChanged(scopedKey, taskFailure)
    val recovered2 = v2.pickle.value.unpickle[protocol.Message] match {
      case v: protocol.ValueChanged => v
      case other => throw new AssertionError(s"expecting ValueChanged got $other")
    }
    recovered2.value.result[Int] match {
      case Failure(e) => e.getMessage must_== "bam"
      case Success(_) => sys.error("unexpected success")
    }
    roundTripMessage(v2)
  }

  @Test
  def testLogEvents: Unit = {
    roundTripMessage(protocol.TaskLogEvent(1, protocol.LogStdOut("Hello, world")))
    roundTripMessage(protocol.CoreLogEvent(protocol.LogStdOut("Hello, world")))
    roundTripMessage(protocol.TaskLogEvent(2, protocol.LogMessage(protocol.LogMessage.INFO, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(3, protocol.LogMessage(protocol.LogMessage.ERROR, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(4, protocol.LogMessage(protocol.LogMessage.WARN, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(5, protocol.LogMessage(protocol.LogMessage.DEBUG, "TEST")))
    roundTripMessage(protocol.TaskLogEvent(6, protocol.LogStdErr("TEST")))
    roundTripMessage(protocol.TaskLogEvent(7, protocol.LogStdOut("TEST2")))
  }

  @Test
  def testTaskEvents: Unit = {
    import protocol.CompilationFailure
    import sbt.serialization.json._
    val taskEvent1: Message = protocol.TaskEvent(8, PlayStartedEvent(port = 10))
    val recovered1 = taskEvent1.pickle.value.unpickle[protocol.Message] match {
      case x: protocol.TaskEvent => x
      case other => throw new AssertionError(s"Expected TaskEvent got $other")
    }
    recovered1 match {
      case PlayStartedEvent(taskId, playStarted) => playStarted.port must_== 10
    }
    roundTripMessage(taskEvent1)

    val taskEvent2: Message = protocol.TaskEvent(9, CompilationFailure(projectRef, nopos, severity, "aww snap"))
    val recovered2 = taskEvent2.pickle.value.unpickle[protocol.Message] match {
      case x: protocol.TaskEvent => x
      case other => throw new AssertionError(s"Expected TaskEvent got $other")
    }
    val failure = recovered2.serialized.parse[CompilationFailure].get
    failure.message must_== "aww snap"
    roundTripMessage(taskEvent2)
  }

  @Test
  def testBackgroundJobEvent: Unit = {
    roundTripMessage(protocol.BackgroundJobStarted(9, protocol.BackgroundJobInfo(id = 67, humanReadableName = "foojob", spawningTask = scopedKey)))
    roundTripMessage(protocol.BackgroundJobFinished(9, 67))

    // TODO these imports should not be needed!
    import PlayStartedEvent.pickler
    import protocol.Message.pickler
    import protocol.Message.unpickler
    val bgje = protocol.BackgroundJobEvent(67, PlayStartedEvent(port = 10))
    val pickled = SpecsUtil.pickleMessage(bgje)
    val recovered3 = SpecsUtil.parseMessage(pickled) match {
      case e: protocol.BackgroundJobEvent => e
      case other => throw new AssertionError("did not unpickle the right thing: " + other)
    }
    recovered3 match {
      case PlayStartedEventBg(taskId, playStarted) => playStarted.port must_== 10
    }
    roundTripMessage(bgje)
  }
}
