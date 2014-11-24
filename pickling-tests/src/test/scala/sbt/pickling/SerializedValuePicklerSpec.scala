package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import scala.pickling._, sbt.pickling.json._
import SpecsUtil._
import JUnitUtil._
import sbt.protocol
/* TODO
import protocol.{ SerializedValue, TaskEventUnapply }

class SerializedValuePicklerTest {
  @Test
  def testRoundtripInt: Unit = {
    val value: SerializedValue = SerializedValue(1)
    value.pickle.value must_== "1.0"
    value.parse[Int].get must_== 1
    roundTrip(SerializedValue(1): SerializedValue)
  }

  @Test
  def testRoundtripPlayStarted: Unit = {
    val value = SerializedValue(PlayStartedEvent(10))
    val example = """{"port":10.0,"$type":"sbt.pickling.spec.PlayStartedEvent"}"""
    value.pickle.value must_== example
    val recovered = example.unpickle[SerializedValue]
    recovered.parse[PlayStartedEvent].get must_== PlayStartedEvent(10)
    roundTrip(SerializedValue(PlayStartedEvent(10)))
  }
}

final case class PlayStartedEvent(port: Int)
object PlayStartedEvent extends protocol.TaskEventUnapply[PlayStartedEvent]
object PlayStartedEventBg extends protocol.BackgroundJobEventUnapply[PlayStartedEvent]
*/
