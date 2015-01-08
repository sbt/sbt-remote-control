package sbt.pickling.spec

import sbt.pickling._
import sbt.pickling.json._
import sbt.serialization._
import sbt.protocol

final case class PlayStartedEvent(port: Int)
object PlayStartedEvent extends protocol.TaskEventUnapply[PlayStartedEvent] {
  implicit val pickler = genPickler[PlayStartedEvent]
  implicit val unpickler = genUnpickler[PlayStartedEvent]
}
object PlayStartedEventBg extends protocol.BackgroundJobEventUnapply[PlayStartedEvent]
