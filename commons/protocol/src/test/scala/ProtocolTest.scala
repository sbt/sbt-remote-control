/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
import sbt.protocol
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import play.api.libs.json.Json

case class PlayStartedEvent(port: Int)
object PlayStartedEvent {
  implicit val format = Json.format[PlayStartedEvent]
}

class ProtocolTest {

  @Test
  def testRawStructure(): Unit = {
    val key = protocol.AttributeKey("name", protocol.TypeInfo("java.lang.String"))
    val build = new java.net.URI("file:///test/project")
    val scope = protocol.SbtScope(project = Some(
      protocol.ProjectReference(build, "test")))
    val scopedKey = protocol.ScopedKey(key, scope)
    val keyFilter = protocol.KeyFilter(Some("test"), Some("test2"), Some("test3"))
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Seq(build),
      projects = Seq(protocol.MinimalProjectStructure(scope.project.get, Seq("com.foo.Plugin"))))

    val specifics = Seq(
      // Requests
      protocol.KillServerRequest(),
      protocol.ReadLineRequest(42, "HI", true),
      protocol.ReadLineResponse(Some("line")),
      protocol.ConfirmRequest(43, "msg"),
      protocol.ConfirmResponse(true),
      protocol.ReceivedResponse(),
      protocol.RequestCompleted(),
      protocol.CommandCompletionsRequest("He", 2),
      protocol.CommandCompletionsResponse(Set(protocol.Completion("llo", "Hello", true))),
      protocol.ListenToEvents(),
      protocol.ListenToBuildChange(),
      protocol.ExecutionRequest("test command string"),
      protocol.ListenToValue(scopedKey),
      protocol.CancelExecutionRequest(1),
      // Responses
      protocol.ErrorResponse("ZOMG"),
      protocol.CancelExecutionResponse(false),
      // Events
      // TODO - CompilationFailure
      protocol.TaskStarted(47, 1, Some(scopedKey)),
      protocol.TaskFinished(48, 1, Some(scopedKey), true),
      protocol.TaskStarted(47, 1, None),
      protocol.TaskFinished(48, 1, None, true),
      protocol.TaskStarted(49, 2, Some(scopedKey)),
      protocol.TaskFinished(50, 2, Some(scopedKey), true),
      protocol.BuildStructureChanged(buildStructure),
      protocol.ValueChanged(scopedKey, protocol.TaskFailure("O NOES")),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue("HI"))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true))),
      // TODO make Unit work ?
      // protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(()))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f))),
      protocol.LogEvent(1, protocol.LogStdOut("Hello, world")),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestOutcome("passed"), None)),
      protocol.ExecutionWaiting(41, "foo", protocol.ClientInfo(java.util.UUID.randomUUID.toString, "foo", "FOO")),
      protocol.ExecutionStarting(56),
      protocol.ExecutionFailure(42),
      protocol.ExecutionSuccess(44),
      protocol.LogEvent(2, protocol.LogMessage(protocol.LogMessage.INFO, "TEST")),
      protocol.LogEvent(3, protocol.LogMessage(protocol.LogMessage.ERROR, "TEST")),
      protocol.LogEvent(4, protocol.LogMessage(protocol.LogMessage.WARN, "TEST")),
      protocol.LogEvent(5, protocol.LogMessage(protocol.LogMessage.DEBUG, "TEST")),
      protocol.LogEvent(6, protocol.LogStdErr("TEST")),
      protocol.LogEvent(7, protocol.LogStdOut("TEST2")),
      protocol.TaskEvent(8, PlayStartedEvent(port = 10)))

    for (s <- specifics) {
      import protocol.WireProtocol.{ fromRaw, toRaw }
      val roundtrippedOption = fromRaw(toRaw(s))
      assertEquals(s"Failed to serialize:\n$s\n\n${toRaw(s)}\n\n", Some(s), roundtrippedOption)
    }

    // check TaskEvent unpacking... not using any nice extractors here
    // because custom unapply() confuses Play's json macros
    protocol.TaskEvent(8, PlayStartedEvent(port = 10)) match {
      case event: protocol.TaskEvent if event.name == "PlayStartedEvent" => {
        protocol.TaskEvent.fromEvent[PlayStartedEvent](event) match {
          case Some((taskId, playStarted)) =>
            assertEquals(8, taskId)
            assertEquals(10, playStarted.port)
          case other => throw new AssertionError("nobody expects PlayStartedEvent to be " + other)
        }
      }
      case other => throw new AssertionError("nobody expects task event to be " + other)
    }
  }

}
