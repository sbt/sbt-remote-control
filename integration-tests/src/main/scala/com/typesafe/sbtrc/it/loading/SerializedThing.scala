package com.typesafe.sbtrc
package it
package loading

final case class SerializedThing(name: String, value: Int)
object SerializedThing {
  import sbt.serialization._
  // TODO - we don't want this to gunky everything up.
  import sbt.protocol.CoreProtocol._

  implicit val pickler: scala.pickling.SPickler[SerializedThing] = scala.pickling.Defaults.genPickler[SerializedThing]
  implicit val unpickler: scala.pickling.Unpickler[SerializedThing] = scala.pickling.Defaults.genUnpickler[SerializedThing]
}
