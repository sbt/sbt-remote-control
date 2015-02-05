package sbt.impl
package ipc

import java.net.{ InetAddress, ServerSocket, Socket }
import java.io.DataInputStream
import java.io.BufferedInputStream
import java.io.DataOutputStream
import java.io.BufferedOutputStream
import java.io.IOException
import java.nio.charset.Charset
import java.io.InputStream
import java.net.SocketException
import java.util.concurrent.atomic.AtomicInteger
import sbt.serialization._

trait Envelope[T] {
  def serial: Long
  def replyTo: Long
  def content: T
}

final case class WireEnvelope(length: Int, override val serial: Long, override val replyTo: Long, override val content: Array[Byte]) extends Envelope[Array[Byte]] {
  def asString: String = {
    new String(content, utf8)
  }
}
/** Thrown if we have issues performing a handshake between client + server. */
class HandshakeException(msg: String, cause: Exception, val socket: Socket) extends Exception(msg, cause)

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

  protected def handshake(toSend: String, toExpect: String): Unit = try {
    sendString(toSend, serialGetAndIncrement())

    val m = receive()
    if (m.serial != 1L) {
      close()
      throw new HandshakeException("Expected handshake serial 1", null, socket)
    }

    val s = m.asString
    if (s != toExpect) {
      close()
      throw new HandshakeException("Expected greeting '" + toExpect + "' received '" + s + "'", null, socket)
    }
  } catch {
    case e: IOException => throw new HandshakeException("Unable to perform handshake", e, socket)
  }

  def isClosed = socket.isClosed()

  // this is not automatic because if you want to use the serial you need
  // to be sure to record it BEFORE you send the message with that serial.
  def serialGetAndIncrement(): Long =
    nextSerial.getAndIncrement()

  def send(message: WireEnvelope): Unit = out.synchronized {
    require(message.serial < nextSerial.get)
    if (isClosed)
      throw new SocketException("socket is closed")
    out.writeInt(message.length)
    out.writeLong(message.serial)
    out.writeLong(message.replyTo)
    out.write(message.content)
    out.flush()
  }

  def send(message: Array[Byte], serial: Long): Unit =
    send(WireEnvelope(length = message.length, serial = serial,
      replyTo = 0L, content = message))

  def reply(replyTo: Long, message: Array[Byte]): Unit = {
    require(replyTo != 0L)
    send(WireEnvelope(length = message.length, serial = serialGetAndIncrement(),
      replyTo = replyTo, content = message))
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

  def sendString(message: String, serial: Long): Unit = {
    send(message.getBytes(utf8), serial)
  }

  def replyString(replyTo: Long, message: String): Unit = {
    require(replyTo != 0L)
    reply(replyTo, message.getBytes(utf8))
  }

  private def jsonString[T: Pickler](message: T): String = {
    SerializedValue(message).toJsonString
  }

  def sendJson[T: Pickler](message: T, serial: Long): Unit = {
    sendString(jsonString(message), serial)
  }

  def replyJson[T: Pickler](replyTo: Long, message: T): Unit = {
    require(replyTo != 0L)
    replyString(replyTo, jsonString(message))
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

class Server(private val serverSocket: ServerSocket) extends MultiClientServer(serverSocket.accept()) {

  handshake(ServerGreeting, ClientGreeting)

  def port = serverSocket.getLocalPort()

  override def close() = {
    super.close()
    ignoringIOException { serverSocket.close() }
  }
}

class MultiClientServer(socket: Socket) extends Peer(socket) {
  handshake(ServerGreeting, ClientGreeting)
}

class Client(socket: Socket) extends Peer(socket) {
  handshake(ClientGreeting, ServerGreeting)
}
