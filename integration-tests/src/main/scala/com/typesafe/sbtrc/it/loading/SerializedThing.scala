package com.typesafe.sbtrc
package it
package loading

final case class SerializedThing(name: String, value: Int)
object SerializedThing {
  import sbt.serialization._
  implicit val pickler = genPickler[SerializedThing]
  implicit val unpickler = genUnpickler[SerializedThing]
}
