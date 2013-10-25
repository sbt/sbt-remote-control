package com.typesafe.sbtrc
package ipc

import java.net.{ InetAddress, ServerSocket, Socket }
import java.io.DataInputStream
import java.io.BufferedInputStream
import java.io.DataOutputStream
import java.io.BufferedOutputStream
import java.io.IOException
import java.nio.charset.Charset
import java.io.InputStream
import scala.util.parsing.json._
import java.net.SocketException
import java.util.concurrent.atomic.AtomicInteger

trait Envelope[T] {
  def serial: Long
  def replyTo: Long
  def content: T
}

case class WireEnvelope(length: Int, override val serial: Long, override val replyTo: Long, override val content: Array[Byte]) extends Envelope[Array[Byte]] {
  def asString: String = {
    new String(content, utf8)
  }
}

trait JsonWriter[-T] {
  def toJson(t: T): JSONType
}

object JsonWriter {
  def toJsonArray[T: JsonWriter](ts: Seq[T]): JSONArray = {
    JSONArray((ts map { t => implicitly[JsonWriter[T]].toJson(t) }).toList)
  }

  implicit def jsonWriter[T](implicit s: protocol.RawStructure[T]): JsonWriter[T] =
    new JsonWriter[T] {
      def toJson(t: T): JSONType =
        addJsonToParams(s(t))
      private def addJsonToAny(value: Any): Any = value match {
        // null is allowed in json
        case null => null
        // keep wrappers but ensure we wrap any nested items
        case JSONObject(v) => addJsonToAny(v)
        case JSONArray(v) => addJsonToAny(v)
        // add wrappers if missing
        case s: Seq[_] => JSONArray(s.map(addJsonToAny(_)).toList)
        case m: Map[_, _] => JSONObject(m map {
          case (key: String, value) =>
            (key -> addJsonToAny(value))
          case whatever =>
            throw new RuntimeException("Invalid map entry in params " + whatever)
        })
        case s: String => s
        case n: Number => n
        case b: Boolean => b
        case whatever => throw new RuntimeException("not allowed in params: " + whatever)
      }

      private def addJsonToParams(params: Any): JSONObject = {
        addJsonToAny(params).asInstanceOf[JSONObject]
      }
    }
}

trait JsonReader[+T] {
  def fromJson(s: JSONType): T
}
object JsonReader {
  implicit def fromRaw[T](implicit s: protocol.RawStructure[T]): JsonReader[T] =
    new JsonReader[T] {
      def fromJson(json: JSONType): T = {
        val resultOpt = s.unapply(cleanJsonFromParams(json))
        resultOpt.getOrElse(sys.error("Unable to deserialize json!"))
      }
      private def cleanJsonFromAny(value: Any): Any = value match {
        // null is allowed in json
        case null => null
        // strip the scala JSON wrappers off, if present; this
        // is basically due to not being sure when Scala's json stuff
        // will use these.
        case JSONObject(v) => cleanJsonFromAny(v)
        case JSONArray(v) => cleanJsonFromAny(v)
        // all sequences must be lists of sanitized values
        case s: Seq[_] => s.map(cleanJsonFromAny(_)).toList
        case m: Map[_, _] => m map {
          case (key: String, value) =>
            (key -> cleanJsonFromAny(value))
          case whatever =>
            throw new RuntimeException("Invalid map entry in params " + whatever)
        }
        case s: String => s
        case n: Number => n
        case b: Boolean => b
        case whatever => throw new RuntimeException("not allowed in params: " + whatever)
      }

      private def cleanJsonFromParams(params: Any): Map[String, Any] = {
        cleanJsonFromAny(params).asInstanceOf[Map[String, Any]]
      }
    }
}

// This is thread-safe in that it should send/receive each message atomically,
// but multiple threads will have to be careful that they don't send messages
// in a nonsensical sequence.
abstract class Peer(protected val socket: Socket) {
  require(!socket.isClosed())
  require(socket.getInputStream() ne null)
  require(socket.getOutputStream() ne null)

  // these two need to be protected by synchronized on the streams
  private val in = new DataInputStream(new BufferedInputStream(socket.getInputStream()))
  private val out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()))

  // this would only be useful if we buffered received messages and
  // allowed replies to be sent out of order
  private val nextSerial = new AtomicInteger(1)

  protected def handshake(toSend: String, toExpect: String): Unit = {
    sendString(toSend)

    val m = receive()
    if (m.serial != 1L) {
      close()
      throw new RuntimeException("Expected handshake serial 1")
    }

    val s = m.asString
    if (s != toExpect) {
      close()
      throw new RuntimeException("Expected greeting '" + toExpect + "' received '" + s + "'")
    }
  }

  def isClosed = socket.isClosed()

  def send(message: WireEnvelope): Unit = out.synchronized {
    if (isClosed)
      throw new SocketException("socket is closed")
    out.writeInt(message.length)
    out.writeLong(message.serial)
    out.writeLong(message.replyTo)
    out.write(message.content)
    out.flush()
  }

  def send(message: Array[Byte]): Long = {
    reply(0L, message)
  }

  def reply(replyTo: Long, message: Array[Byte]): Long = {
    val serial = nextSerial.getAndIncrement()
    send(WireEnvelope(message.length, serial, replyTo, message))
    serial
  }

  def receive(): WireEnvelope = in.synchronized {
    if (isClosed)
      throw new SocketException("socket is closed")
    val length = in.readInt()
    val serial = in.readLong()
    val replyTo = in.readLong()
    if (length > (1024 * 1024))
      throw new RuntimeException("Ridiculously huge message (" + length + " bytes)")
    val bytes = new Array[Byte](length)
    in.readFully(bytes)
    WireEnvelope(length, serial, replyTo, bytes)
  }

  def sendString(message: String): Long = {
    send(message.getBytes(utf8))
  }

  def replyString(replyTo: Long, message: String): Long = {
    reply(replyTo, message.getBytes(utf8))
  }

  def sendJson[T: JsonWriter](message: T): Long = {
    sendString(implicitly[JsonWriter[T]].toJson(message).toString)
  }

  def replyJson[T: JsonWriter](replyTo: Long, message: T): Long = {
    replyString(replyTo, implicitly[JsonWriter[T]].toJson(message).toString)
  }

  def close(): Unit = {
    // don't synchronize the close() calls, we need to be able
    // to close from another thread (and we're assuming that
    // Java streams are OK with that)
    ignoringIOException { in.close() }
    ignoringIOException { out.close() }
    ignoringIOException { socket.close() }
  }
}

class Server(private val serverSocket: ServerSocket) extends Peer(serverSocket.accept()) {

  handshake(ServerGreeting, ClientGreeting)

  def port = serverSocket.getLocalPort()

  override def close() = {
    super.close()
    ignoringIOException { serverSocket.close() }
  }
}

class Client(socket: Socket) extends Peer(socket) {
  handshake(ClientGreeting, ServerGreeting)
}
