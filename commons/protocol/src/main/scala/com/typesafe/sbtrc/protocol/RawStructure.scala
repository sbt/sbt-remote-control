package com.typesafe.sbtrc.protocol

import com.typesafe.sbtrc.ipc
import scala.util.parsing.json._
import com.typesafe.sbtrc.ipc.JsonReader


// Hacky, Hacky typeclass to work with Scala's raw JSON library
// (so the same code can be used on multiple scala versions, yay).
trait RawStructure[T] {
  def apply(t: T): Map[String, Any]
  def unapply(map: Map[String, Any]): Option[T]
}
object JsonStructure {
  def apply[T](t: T)(implicit real: RawStructure[T]): Map[String, Any] =
    real(t)
  def unapply[T](map: Map[String, Any])(implicit real: RawStructure[T]): Option[T] =
    real.unapply(map)  
}