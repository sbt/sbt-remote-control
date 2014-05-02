package com.typesafe.sbtrc
package it
package loading

import play.api.libs.json.Json

case class SerializedThing(name: String, value: Int)
object SerializedThing {
  implicit val format = Json.format[SerializedThing]
}