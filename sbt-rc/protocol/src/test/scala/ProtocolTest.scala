/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import com.typesafe.sbtrc.protocol.SpecificMessage
import com.typesafe.sbtrc.protocol.GenericMessage

class ProtocolTest {

  @Test
  def testGenericSpecific(): Unit = {
    val specifics = Seq(
      protocol.RunResponse(success = true, task = protocol.TaskNames.run),
      protocol.RunResponse(success = false, task = protocol.TaskNames.runMain))
    for (s <- specifics) {
      val roundtrippedOption = s.toGeneric.toSpecific
      assertEquals(Some(s), roundtrippedOption)
    }

    // and through json
    for (s <- specifics) {
      val roundtrippedOption =
        protocol.Message.JsonRepresentationOfMessage.fromJson(protocol.Message.JsonRepresentationOfMessage.toJson(s.toGeneric)) match {
          case m: GenericMessage => m.toSpecific
          case whatever => throw new AssertionError("got wrong json parse result: " + whatever)
        }
      assertEquals(Some(s), roundtrippedOption)
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
          case protocol.Envelope(serial, replyTo, protocol.GenericRequest(_, protocol.TaskNames.name, _)) =>
            client.replyJson(serial, protocol.LogEvent(protocol.LogMessage("info", "a message")))
            client.replyJson(serial, protocol.NameResponse("foobar"))
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
          case protocol.Envelope(serial, replyTo, r: protocol.GenericResponse) => r.params("name").asInstanceOf[String]
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
          case protocol.Envelope(serial, replyTo, protocol.GenericRequest(_, protocol.TaskNames.discoveredMainClasses, _)) =>
            client.replyJson(serial, protocol.DiscoveredMainClassesResponse(Nil))
          case protocol.Envelope(serial, replyTo, other) =>
            client.replyJson(serial, protocol.ErrorResponse("did not understand request: " + other))
        }
      },
      { (server) =>
        server.sendJson(protocol.DiscoveredMainClassesRequest(sendEvents = true))
        val names = protocol.Envelope(server.receive()) match {
          case protocol.Envelope(serial, replyTo, r: protocol.GenericResponse) => r.params("names").asInstanceOf[Seq[String]]
          case protocol.Envelope(serial, replyTo, r) =>
            throw new AssertionError("unexpected response: " + r)
        }
        assertTrue("no names returned", names.isEmpty)
      })
  }
}
