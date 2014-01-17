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


class ProtocolTest {

  @Test
  def testRawStructure(): Unit = {
    val key = protocol.AttributeKey("name", protocol.TypeInfo("java.lang.String"))
    val build = new java.net.URI("file:///test/project")
    val scope = protocol.SbtScope(project=Some(
        protocol.ProjectReference(build, "test")))
    val scopedKey = protocol.ScopedKey(key, scope)
    val keyFilter = protocol.KeyFilter(Some("test"), Some("test2"), Some("test3"))
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Seq(build),
      projects = Seq(scope.project.get)
    )
    val specifics = Seq(
      // Requests
      protocol.ListenToEvents(),
      protocol.ListenToBuildChange(),
      protocol.ExecutionRequest("test command string"),
      protocol.ListenToValue(scopedKey),
      // Responses
      protocol.ErrorResponse("ZOMG"),
      // Events
      // TODO - CompilationFailure
      protocol.TaskStarted(scopedKey),
      protocol.TaskFinished(scopedKey, true),
      protocol.NeedRebootEvent,
      protocol.NowListeningEvent,
      protocol.TaskStarted(scopedKey),
      protocol.TaskFinished(scopedKey, true),
      protocol.BuildStructureChanged(buildStructure),
      protocol.ValueChange(scopedKey, protocol.TaskFailure("O NOES")),
      protocol.ValueChange(scopedKey, protocol.TaskSuccess(protocol.BuildValue("HI"))),
      protocol.LogEvent(protocol.LogStdOut("Hello, world")),
      protocol.TestEvent("name", None, protocol.TestOutcome("passed"), None),
      protocol.ExecutionDone("test result command")
      // TODO - protocol.GenericEvent("playServerStarted", Map("port" -> 10))
    )
    for (s <- specifics) {
      import protocol.WireProtocol.{fromRaw,toRaw}
      val roundtrippedOption = fromRaw(toRaw(s))
      assertEquals(s"Failed to serialize:\n$s\n\n${toRaw(s)}\n\n", Some(s), roundtrippedOption)
    }
  }

 
}
