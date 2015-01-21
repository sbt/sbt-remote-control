package com.typesafe.sbtrc
package it
package loading

final case class SerializedThing(name: String, value: Int)
object SerializedThing {
  import sbt.serialization._
  // TODO - we don't want this to gunky everything up.
  import scala.pickling.Defaults._
  implicit val pickler = genPickler[SerializedThing]
  implicit val unpickler = genUnpickler[SerializedThing]
}
