package com.typesafe.sbtrc
package it
package loading

final case class SerializedThing(name: String, value: Int)
object SerializedThing {
  import sbt.serialization._
  // TODO - we don't want this to gunky everything up.

  implicit val pickler: Pickler[SerializedThing] = Pickler.generate[SerializedThing]
  implicit val unpickler: Unpickler[SerializedThing] = Unpickler.generate[SerializedThing]
}
