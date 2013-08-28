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
object RawStructure {
  def get[T](implicit result: RawStructure[T]): RawStructure[T] = result
  
  
  implicit object StringStructure extends RawStructure[String] {
    def apply(t: String): Map[String, Any] =
      Map("value" -> t)
    def unapply(map: Map[String, Any]): Option[String] =
      map.get("value").map(_.toString)
  }
  
  implicit object FileStructure extends RawStructure[java.io.File] {
    def apply(t: java.io.File): Map[String, Any] =
      Map("value" -> t.toURI.toASCIIString())
    def unapply(map: Map[String, Any]): Option[java.io.File] =
      map.get("value").map(uri => new java.io.File(new java.net.URI(uri.toString)))
  }
  
  implicit object BooleanStructure extends RawStructure[Boolean] {
    def apply(t: Boolean): Map[String, Any] =
      Map("value" -> t)
    def unapply(map: Map[String, Any]): Option[Boolean] =
      map.get("value").map(t => t.asInstanceOf[Boolean])
  }
  
  implicit def SeqStructure[T](implicit values: RawStructure[T]) =
    new RawStructure[Seq[T]] {
      def apply(t: Seq[T]): Map[String, Any] =
        Map("values" -> t.map(values.apply))
      def unapply(map: Map[String, Any]): Option[Seq[T]] =
        map.get("values").map { rawSeq =>
          rawSeq.asInstanceOf[Seq[Map[String,Any]]].flatMap { raw => 
            values.unapply(raw).toSeq
          }
        }
    }
}
object JsonStructure {
  def apply[T](t: T)(implicit real: RawStructure[T]): Map[String, Any] =
    real(t)
  def unapply[T](map: Map[String, Any])(implicit real: RawStructure[T]): Option[T] =
    real.unapply(map)  
}