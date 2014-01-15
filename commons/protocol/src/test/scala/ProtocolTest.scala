/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
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
      protocol.TaskStarted(scopedKey),
      protocol.TaskFinished(scopedKey, true),
      protocol.BuildStructureChanged(buildStructure),
      protocol.ExecutionDone("test result command"),
      protocol.ListenToEvents(),
      protocol.ListenToBuildChange(),
      protocol.ExecutionRequest("test command string"),
      // Events
      protocol.Started,
      protocol.Stopped,
      protocol.NeedRebootEvent,
      protocol.NowListeningEvent,
      protocol.RequestReceivedEvent,
      protocol.LogEvent(protocol.LogStdOut("Hello, world")),
      protocol.TestEvent("name", None, protocol.TestOutcome("passed"), None),
      protocol.GenericEvent("playServerStarted", Map("port" -> 10)),
      // Generic API
      protocol.ErrorResponse("ZOMG"),
      protocol.SettingKeyRequest(keyFilter),
      protocol.TaskKeyRequest(keyFilter),
      protocol.InputTaskKeyRequest(keyFilter),
      protocol.SettingValueRequest(scopedKey),
      protocol.TaskValueRequest(scopedKey),
      protocol.KeyListResponse(protocol.KeyList(Seq(scopedKey))),
      protocol.KeyListResponse(protocol.KeyList(Nil)),
      protocol.TaskValueResponse(protocol.TaskSuccess(protocol.BuildValue("Hey"))),
      protocol.ExecuteCommandRequest("hello"),
      protocol.ExecuteCommandResponse(),
      protocol.SettingValueResponse(protocol.TaskFailure("O NOES")),
      // High level API
      protocol.NameRequest(true),
      protocol.NameResponse(Seq(
          protocol.ProjectInfo(
            protocol.ProjectReference(new java.net.URI("file://sample/"), "jtest"),
            "Johnny Test",
            true,
            Map("awesome" -> true)
          )
      )),
      protocol.MainClassRequest(false),
      protocol.MainClassResponse(Seq(
        protocol.DiscoveredMainClasses(
          protocol.ProjectReference(new java.net.URI("file://sample/"), "jtest"),
          Seq("test.Main", "test.Main2"),
          Some("test.Main")
        )
      )),
      protocol.WatchTransitiveSourcesRequest(true),
      protocol.WatchTransitiveSourcesResponse(Seq(new java.io.File(".").getAbsoluteFile)),
      protocol.CompileRequest(true, ref = Some(protocol.ProjectReference(new java.net.URI("file://temp"), "test"))),
      protocol.CompileResponse(Seq(
         protocol.CompileResult(
           protocol.ProjectReference(new java.net.URI("file://temp"), "test"),
           success = false
         )
      )),
      protocol.RunRequest(
          sendEvents = true,
          ref = Some(protocol.ProjectReference(new java.net.URI("file://temp"), "test")),
          mainClass = Some("hi"), 
          useAtmos = true),
      protocol.RunRequest(sendEvents = false, mainClass = None, useAtmos = false),
      protocol.RunResponse(success = true, task = "run"),
      protocol.RunResponse(success = false, task = "run-main"),
      protocol.TestRequest(true),
      protocol.TestResponse(protocol.TestError)
    )
    for (s <- specifics) {
      val struct = com.typesafe.sbtrc.protocol.WireProtocol.MessageStructure
      val roundtrippedOption = struct unapply struct(s)
      assertEquals("Failed to serialize: " + s, Some(s), roundtrippedOption)
    }

    // and through json
    for (s <- specifics) {
      val struct = com.typesafe.sbtrc.protocol.WireProtocol.MessageStructure
      val writer = com.typesafe.sbtrc.ipc.JsonWriter.jsonWriter(struct)
      val reader = com.typesafe.sbtrc.ipc.JsonReader.fromRaw(struct)
      val roundtrippedOption = reader.fromJson(writer.toJson(s)) 
      assertEquals(s, roundtrippedOption)
    }
  }

 
}
