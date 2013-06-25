/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
import java.io.File
import akka.util.ByteString

class ByteStringDecoderTest {

  val utf8Bytes = {
    val in = getClass.getResourceAsStream("UTF-8-demo.txt")
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
    val decoder = new ByteStringDecoder()
    decoder.feed(ByteString(utf8Bytes))
    decoder.finish()
    val result = decoder.read.mkString
    assertEquals(utf8String.size, result.size)
    assertEquals(utf8String, result)
  }

  @Test
  def testDecoderOneByteChunks(): Unit = {
    val decoder = new ByteStringDecoder()
    for (i <- 0 until utf8Bytes.length) {
      decoder.feed(ByteString.fromArray(utf8Bytes, i, 1))
    }
    decoder.finish()
    val result = decoder.read.mkString
    assertEquals(utf8String.size, result.size)
    assertEquals(utf8String, result)
  }
}
