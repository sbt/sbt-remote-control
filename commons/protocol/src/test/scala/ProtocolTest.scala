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
import play.api.libs.json._

case class PlayStartedEvent(port: Int)
object PlayStartedEvent extends protocol.TaskEventUnapply[PlayStartedEvent] {
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

    val serializations = protocol.DynamicSerialization.defaultSerializations

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
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue("HI", serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42, serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L, serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true, serializations))),
      // TODO make Unit work ?
      // protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(()))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0, serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f, serializations))),
      protocol.TaskLogEvent(1, protocol.LogStdOut("Hello, world")),
      protocol.CoreLogEvent(protocol.LogStdOut("Hello, world")),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestOutcome("passed"), None)),
      protocol.ExecutionWaiting(41, "foo", protocol.ClientInfo(java.util.UUID.randomUUID.toString, "foo", "FOO")),
      protocol.ExecutionStarting(56),
      protocol.ExecutionFailure(42),
      protocol.ExecutionSuccess(44),
      protocol.TaskLogEvent(2, protocol.LogMessage(protocol.LogMessage.INFO, "TEST")),
      protocol.TaskLogEvent(3, protocol.LogMessage(protocol.LogMessage.ERROR, "TEST")),
      protocol.TaskLogEvent(4, protocol.LogMessage(protocol.LogMessage.WARN, "TEST")),
      protocol.TaskLogEvent(5, protocol.LogMessage(protocol.LogMessage.DEBUG, "TEST")),
      protocol.TaskLogEvent(6, protocol.LogStdErr("TEST")),
      protocol.TaskLogEvent(7, protocol.LogStdOut("TEST2")),
      protocol.TaskEvent(8, PlayStartedEvent(port = 10)))

    for (s <- specifics) {
      import protocol.WireProtocol.{ fromRaw, toRaw }
      val roundtrippedOption = fromRaw(toRaw(s), protocol.DynamicSerialization.defaultSerializations)
      assertEquals(s"Failed to serialize:\n$s\n\n${toRaw(s)}\n\n", Some(s), roundtrippedOption)
    }

    protocol.TaskEvent(4, protocol.TestEvent("name", Some("foo"), protocol.TestOutcome("passed"), Some("bar"))) match {
      case protocol.TestEvent(taskId, test) =>
        assertEquals(4, taskId)
        assertEquals("name", test.name)
        assertEquals(Some("foo"), test.description)
        assertEquals(Some("bar"), test.error)
    }

    // check TaskEvent unpacking using TaskEventUnapply
    protocol.TaskEvent(8, PlayStartedEvent(port = 10)) match {
      case PlayStartedEvent(taskId, playStarted) =>
        assertEquals(8, taskId)
        assertEquals(10, playStarted.port)
      case other => throw new AssertionError("nobody expects PlayStartedEvent to be " + other)
    }
  }

  @Test
  def testDynamicSerialization(): Unit = {
    val ds = protocol.DynamicSerialization.defaultSerializations
    def roundtrip[T: Manifest](t: T): Unit = {
      val formatOption = ds.lookup(implicitly[Manifest[T]])
      formatOption map { format =>
        val json = format.writes(t)
        //System.err.println(s"${t} = ${Json.prettyPrint(json)}")
        val parsed = format.reads(json).asOpt.getOrElse(throw new AssertionError(s"could not re-parse ${json} for ${t}"))
        assertEquals("round trip of " + t, t, parsed)
      } getOrElse { throw new AssertionError(s"No dynamic serialization for $t") }
    }

    roundtrip("Foo")
    roundtrip(new java.io.File("/tmp"))
    roundtrip(true)
    roundtrip(false)
    roundtrip(10: Short)
    roundtrip(11)
    roundtrip(12L)
    roundtrip(13.0f)
    roundtrip(14.0)
    roundtrip(None: Option[String])
    roundtrip(Some("Foo"))
    roundtrip(Some(true))
    roundtrip(Some(10))
    roundtrip(Nil: Seq[String])
    roundtrip(Seq("Bar", "Baz"))
    roundtrip(Seq(1, 2, 3))
    roundtrip(Seq(true, false, true, true, false))
  }

  @Test
  def testBuildValueSerialization(): Unit = {
    val serializations = protocol.DynamicSerialization.defaultSerializations
    implicit def format[T]: Format[protocol.BuildValue[T]] = protocol.BuildValue.format[T](serializations)
    def roundtripBuildValue[T: Manifest](buildValue: protocol.BuildValue[T]): Unit = {
      val json = Json.toJson(buildValue)
      //System.err.println(s"${buildValue} = ${Json.prettyPrint(json)}")
      val parsed = Json.fromJson[protocol.BuildValue[T]](json).asOpt.getOrElse(throw new AssertionError(s"Failed to parse ${buildValue} serialization ${json}"))
      assertEquals(buildValue, parsed)
    }
    def roundtrip[T: Manifest](t: T): Unit = {
      roundtripBuildValue(protocol.BuildValue(t, serializations))
    }
    roundtrip("Foo")
    roundtrip(new java.io.File("/tmp"))
    roundtrip(true)
    roundtrip(false)
    roundtrip(10: Short)
    roundtrip(11)
    roundtrip(12L)
    roundtrip(13.0f)
    roundtrip(14.0)
    roundtrip(None: Option[String])
    roundtrip(Some("Foo"))
    roundtrip(Some(true))
    roundtrip(Some(10))
    roundtrip(Nil: Seq[String])
    roundtrip(Seq("Bar", "Baz"))
    roundtrip(Seq(1, 2, 3))
    roundtrip(Seq(true, false, true, true, false))
  }
}
