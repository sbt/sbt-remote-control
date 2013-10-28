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
    val specifics = Seq(
      // Events
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
      protocol.CompileRequest(true),
      protocol.CompileResponse(false),
      protocol.RunRequest(sendEvents = true, mainClass = Some("hi"), useAtmos = true),
      protocol.RunRequest(sendEvents = false, mainClass = None, useAtmos = false),
      protocol.RunResponse(success = true, task = "run"),
      protocol.RunResponse(success = false, task = "run-main"),
      protocol.TestRequest(true),
      protocol.TestResponse(protocol.TestError)
    )
    for (s <- specifics) {
      val struct = com.typesafe.sbtrc.protocol.Envelope.MessageStructure
      val roundtrippedOption = struct unapply struct(s)
      assertEquals("Failed to serialize: " + s, Some(s), roundtrippedOption)
    }

    // and through json
    for (s <- specifics) {
      val struct = com.typesafe.sbtrc.protocol.Envelope.MessageStructure
      val writer = com.typesafe.sbtrc.ipc.JsonWriter.jsonWriter(struct)
      val reader = com.typesafe.sbtrc.ipc.JsonReader.fromRaw(struct)
      val roundtrippedOption = reader.fromJson(writer.toJson(s)) 
      assertEquals(s, roundtrippedOption)
    }
  }

  private def testClientServer(clientBlock: (ipc.Client) => Unit,
    serverBlock: (ipc.Server) => Unit) = {
    val executor = Executors.newCachedThreadPool()
    val serverSocket = ipc.openServerSocket()
    val port = serverSocket.getLocalPort()
    val latch = new CountDownLatch(2)

    var fail: Option[Exception] = None
    var ok = false

    executor.execute(new Runnable() {
      override def run() = {
        try {
          val server = ipc.accept(serverSocket)

          try {
            serverBlock(server)
          } finally {
            server.close()
          }

          ok = true

        } catch {
          case e: Exception => fail = Some(e)
        } finally {
          latch.countDown()
        }
      }
    })

    executor.execute(new Runnable() {
      override def run() = {
        try {
          val client = ipc.openClient(port)

          try {
            clientBlock(client)
          } finally {
            client.close()
          }

        } catch {
          case e: Exception => fail = Some(e)
        } finally {
          latch.countDown()
        }
      }
    })

    latch.await()

    executor.shutdown()
    executor.awaitTermination(1000, TimeUnit.MILLISECONDS)

    fail foreach { e => throw e }

    assertTrue(ok)
  }

  @Test
  def testRetrieveProjectName(): Unit = {
    testClientServer(
      { (client) =>
        protocol.Envelope(client.receive()) match {
          case protocol.Envelope(serial, replyTo, protocol.NameRequest(true)) =>
            client.replyJson(serial, protocol.LogEvent(protocol.LogMessage("info", "a message")))
            client.replyJson(serial, protocol.NameResponse(Seq(
          protocol.ProjectInfo(
            protocol.ProjectReference(new java.net.URI("file://sample/"), "jtest"),
            "foobar",
            true,
            Map("awesome" -> true)
          )
      )))
          case protocol.Envelope(serial, replyTo, other) =>
            client.replyJson(serial, protocol.ErrorResponse("did not understand request: " + other))
        }
      },
      { (server) =>
        server.sendJson(protocol.NameRequest(sendEvents = true))
        val logMessage = protocol.Envelope(server.receive()) match {
          case protocol.Envelope(serial, replyTo, r: protocol.LogEvent) => r.entry.message
          case protocol.Envelope(serial, replyTo, r) =>
            throw new AssertionError("unexpected response: " + r)
        }
        assertEquals("a message", logMessage)
        val name = protocol.Envelope(server.receive()) match {
          case protocol.Envelope(serial, replyTo, protocol.NameResponse(Seq(
            protocol.ProjectInfo(_, name, _, _)    
          ))) => name
          case protocol.Envelope(serial, replyTo, r) =>
            throw new AssertionError("unexpected response: " + r)
        }
        assertEquals("foobar", name)
      })
  }

  @Test
  def testRetrieveEmptyMainClasses(): Unit = {
    testClientServer(
      { (client) =>
        protocol.Envelope(client.receive()) match {
          case protocol.Envelope(serial, replyTo, _:protocol.MainClassRequest) =>
            client.replyJson(serial, protocol.MainClassResponse(Nil))
          case protocol.Envelope(serial, replyTo, other) =>
            client.replyJson(serial, protocol.ErrorResponse("did not understand request: " + other))
        }
      },
      { (server) =>
        server.sendJson(protocol.MainClassRequest(sendEvents = true))
        val names = protocol.Envelope(server.receive()) match {
          case protocol.Envelope(serial, replyTo, r: protocol.MainClassResponse) => r.projects
          case protocol.Envelope(serial, replyTo, r) =>
            throw new AssertionError("unexpected response: " + r)
        }
        assertTrue("no names returned", names.isEmpty)
      })
  }
}
