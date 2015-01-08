package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import SpecsUtil._
import JUnitUtil._
import sbt.protocol

import sbt.pickling._
import sbt.pickling.json._
import protocol.TaskEventUnapply

import sbt.serialization._
// TODO these are required, despite the wildcard import above;
// no idea why. Try removing them and see if we can still build.
import sbt.serialization.intPicklerUnpickler

class SerializedValuePicklerTest {

  @Test
  def testRoundtripInt: Unit = {
    import scala.pickling._
    val value: SerializedValue = SerializedValue(1)
    value.pickle.value must_== "1.0"
    value.parse[Int].get must_== 1
    roundTrip(SerializedValue(1): SerializedValue)
  }

  @Test
  def testRoundtripPlayStarted: Unit = {
    import scala.pickling._
    val value = SerializedValue(PlayStartedEvent(10))
    val example = """{"port":10.0,"$type":"sbt.pickling.spec.PlayStartedEvent"}"""
    value.pickle.value must_== example
    val recovered = example.unpickle[SerializedValue]
    recovered.parse[PlayStartedEvent].get must_== PlayStartedEvent(10)
    roundTrip(SerializedValue(PlayStartedEvent(10)))
  }
}
