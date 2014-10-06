/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
package sbt.server

import org.junit.Assert._
import org.junit._
import java.io.File

class ByteDecoderTest {

  val utf8Bytes = {
    val in = getClass.getResourceAsStream("/UTF-8-demo.txt")
    if (in == null)
      throw new Exception("Missing UTF-8 test file")
    val size = in.available()
    val array = new Array[Byte](size)
    if (in.read(array) != size)
      throw new Exception("did not read " + size + " files")
    array
  }

  val utf8String = new String(utf8Bytes, "UTF-8")

  @Test
  def testDecoderSingleChunk(): Unit = {
    val decoder = new ByteDecoder()
    decoder.feed(utf8Bytes)
    decoder.finish()
    val result = decoder.read.mkString
    assertEquals(utf8String.size, result.size)
    assertEquals(utf8String, result)
  }

  @Test
  def testDecoderOneByteChunks(): Unit = {
    val decoder = new ByteDecoder()
    for (i <- 0 until utf8Bytes.length) {
      decoder.feed(utf8Bytes(i): Byte)
    }
    decoder.finish()
    val result = decoder.read.mkString
    assertEquals(utf8String.size, result.size)
    assertEquals(utf8String, result)
  }

  @Test
  def testDecoderVariousByteChunks(): Unit = {
    for (chunkSize <- 1 until 16) {
      val decoder = new ByteDecoder()
      for (chunk <- utf8Bytes.sliding(chunkSize, chunkSize)) {
        decoder.feed(chunk: Array[Byte])
      }
      decoder.finish()
      val result = decoder.read.mkString
      assertEquals(utf8String.size, result.size)
      assertEquals(utf8String, result)
    }
  }
}
