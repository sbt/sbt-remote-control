package sbt.protocol.spec

import sbt.protocol
import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import SpecsUtil._
import sbt.serialization.spec.JUnitUtil._
import sbt.serialization._
import protocol.TaskEventUnapply

import scala.pickling.Defaults.pickleOps

class SerializedValuePicklerTest {

  @Test
  def testRoundtripInt: Unit = {
    import scala.pickling._
    val value: SerializedValue = SerializedValue(1)
    SerializedValue(value).toJsonString must_== "1.0"
    value.parse[Int].get must_== 1
    roundTrip(SerializedValue(1): SerializedValue)
  }

  @Test
  def testRoundtripPlayStarted: Unit = {
    import scala.pickling._
    val value = SerializedValue(PlayStartedEvent(10))
    val example = """{"$type":"sbt.protocol.spec.PlayStartedEvent","port":10.0}"""
    SerializedValue(value).toJsonString must_== example
    val recovered = SerializedValue.fromJsonString(example)
    recovered.parse[PlayStartedEvent].get must_== PlayStartedEvent(10)
    roundTrip(SerializedValue(PlayStartedEvent(10)))
  }
}
