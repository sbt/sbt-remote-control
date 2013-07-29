package com.typesafe.sbtrc

import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction
import java.nio.ByteBuffer
import java.nio.CharBuffer
import akka.util.ByteString
import java.nio.charset.CoderResult

// this is a stateful mutable class because CharsetDecoder is anyway
// and can't come up with an easy way to avoid exposing that
class ByteStringDecoder {
  private val decoder = Charset.forName("UTF-8").newDecoder()
    .onMalformedInput(CodingErrorAction.REPLACE)
    .onUnmappableCharacter(CodingErrorAction.REPLACE)
  private var in = ByteBuffer.allocate(0)
  private var out = CharBuffer.allocate(1024)
  private var decoded: Vector[String] = Vector.empty

  private def clearOutBuffer(): Unit = {
    if (out.position > 0) {
      out.flip()
      decoded = decoded :+ out.toString
    }
    out.clear()
  }

  private def decodeLoop(endOfInput: Boolean): Unit = {
    val result = decoder.decode(in, out, endOfInput)
    result match {
      case CoderResult.UNDERFLOW =>
      // there may be bytes left in input but we can't process them yet
      case CoderResult.OVERFLOW =>
        // not enough space in the output buffer, so clear it out and
        // try again...
        clearOutBuffer()
        decodeLoop(endOfInput)
    }
  }

  def feed(bytes: ByteString): Unit = {
    if (in.remaining == 0) {
      in = bytes.asByteBuffer
    } else {
      val toAdd = bytes.asByteBuffer
      val combined = ByteBuffer.allocate(in.remaining + toAdd.remaining)
      combined.put(in)
      combined.put(toAdd)
      combined.flip()
      in = combined
    }

    decodeLoop(endOfInput = false)
  }

  def finish(): Unit = {
    decodeLoop(endOfInput = true)
    clearOutBuffer()

    val result = decoder.flush(out)
    result match {
      case CoderResult.UNDERFLOW =>
      case CoderResult.OVERFLOW =>
        throw new Exception("should not be 256 chars created by flush")
    }

    clearOutBuffer()
  }

  def read(): Seq[String] = {
    clearOutBuffer()

    val result = decoded
    decoded = Vector.empty
    result
  }
}
