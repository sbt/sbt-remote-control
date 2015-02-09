package sbt.protocol.spec

import sbt.protocol.{ TaskEventUnapply, BackgroundJobEventUnapply, DetachedEventUnapply }
import sbt.serialization._

final case class PlayStartedEvent(port: Int)
object PlayStartedEvent extends TaskEventUnapply[PlayStartedEvent] {
  implicit val pickler = genPickler[PlayStartedEvent]
  implicit val unpickler = genUnpickler[PlayStartedEvent]
}
object PlayStartedEventBg extends BackgroundJobEventUnapply[PlayStartedEvent]
object PlayStartedEventDetached extends DetachedEventUnapply[PlayStartedEvent]
