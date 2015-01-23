package com.typesafe.sbtrc
package it
package loading

final case class SerializedThing(name: String, value: Int)
object SerializedThing {
  import sbt.serialization._
  // TODO - we don't want this to gunky everything up.
  import sbt.protocol.CoreProtocol._

  implicit val pickler: SPickler[SerializedThing] = SPickler.generate[SerializedThing]
  implicit val unpickler: Unpickler[SerializedThing] = Unpickler.generate[SerializedThing]
}
