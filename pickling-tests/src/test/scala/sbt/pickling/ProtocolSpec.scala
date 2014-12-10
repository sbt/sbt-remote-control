package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import scala.pickling._, sbt.pickling.json._
import SpecsUtil._
import JUnitUtil._
import sbt.protocol
import sbt.protocol.Message
import scala.pickling.internal.AppliedType
import xsbti.Severity.{ Info, Warn, Error }
import scala.util.{Try, Success, Failure}

class ProtocolTest {
  val key = protocol.AttributeKey("name", AppliedType.parse("java.lang.String")._1)
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
    val v1 = protocol.ValueChanged(scopedKey, taskSuccess)
    val recovered1 = v1.pickle.value.unpickle[protocol.ValueChanged]
    recovered1.value.result[String] match {
      case Success("HI") => ()
      case _             => sys.error("unexpected failure")
    }
    roundTripMessage(v1)

    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0))))
    roundTripMessage(protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f))))
    val taskFailure = protocol.TaskFailure(protocol.BuildValue(new Exception("bam"): Throwable))
    val v2 = protocol.ValueChanged(scopedKey, taskFailure)
    val recovered2 = v2.pickle.value.unpickle[protocol.ValueChanged]
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
    val taskEvent1 = protocol.TaskEvent(8, PlayStartedEvent(port = 10))
    val recovered1 = taskEvent1.pickle.value.unpickle[protocol.TaskEvent]
    recovered1 match {
      case PlayStartedEvent(taskId, PlayStartedEvent(port)) => port must_== 10
    }
    roundTripMessage(taskEvent1)

    val taskEvent2 = protocol.TaskEvent(9, CompilationFailure(projectRef, nopos, severity, "aww snap"))
    val recovered2 = taskEvent2.pickle.value.unpickle[protocol.TaskEvent]
    val failure = recovered2.serialized.parse[CompilationFailure].get
    failure.message must_== "aww snap"
    roundTripMessage(taskEvent2)
  }

  @Test
  def testBackgroundJobEvent: Unit = {
    roundTripMessage(protocol.BackgroundJobStarted(9, protocol.BackgroundJobInfo(id = 67, humanReadableName = "foojob", spawningTask = scopedKey)))
    roundTripMessage(protocol.BackgroundJobFinished(9, 67))

    val bgje = protocol.BackgroundJobEvent(67, PlayStartedEvent(port = 10))
    val recovered3 = bgje.pickle.value.unpickle[protocol.BackgroundJobEvent]
    recovered3 match {
      case PlayStartedEventBg(taskId, PlayStartedEvent(port)) => port must_== 10
    }
    roundTripMessage(bgje)
  }
}
