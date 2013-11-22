package com.typesafe.sbtrc.protocol

import com.typesafe.sbtrc.ipc
import scala.util.parsing.json._
import com.typesafe.sbtrc.ipc.JsonReader


/** Hacky, Hacky typeclass to work with Scala's raw JSON library
 *(so the same code can be used on multiple scala versions, yay).
 * 
 *  A RawStructure is a class decomposed into "basic" types supported by json, that is:
 *  
 *  Map[String,Any] -> Object
 *  Seq[Any]        -> Array
 *  String          -> String
 *  Int, Double     -> Number
 *  Boolean         -> Boolean
 *  null            -> null
 * 
 * This is the mechanism whereby we serialize/deserialze from JSON.  We use this
 * low-level of an API so we can remain JSON library agnostic (and to some extent
 * serialization format agnostic) and still be able to decompose/reconstitute messages.
 * 
 * Hopefully at some point we can migrate into an alternative which is a bit easier to work with.
 * 
 * Play's JSON library is quite nice for this sort of thing, but is not source compatible between
 * Scala 2.10, 2.9 and 2.11.
 * 
 * 
 * TODO - To be "true" to JSON, this should probably create an Any and extract from Any.
 */
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
    override def toString = "RawStructure[File]"
  }
  
  implicit object BooleanStructure extends RawStructure[Boolean] {
    def apply(t: Boolean): Map[String, Any] =
      Map("value" -> t)
    def unapply(map: Map[String, Any]): Option[Boolean] =
      map.get("value").map(t => t.asInstanceOf[Boolean])
    override def toString = "RawStructure[Boolean]"
  }
  
  implicit def SeqStructure[T](implicit valueStructure: RawStructure[T]) =
    new RawStructure[Seq[T]] {
      def apply(t: Seq[T]): Map[String, Any] =
        Map("values" -> t.map(valueStructure.apply))
      def unapply(map: Map[String, Any]): Option[Seq[T]] =
        map.get("values").map { rawSeq =>
          rawSeq.asInstanceOf[Seq[Map[String,Any]]].flatMap { raw => 
            valueStructure.unapply(raw).toSeq
          }
        }
      override def toString = "RawStructure[Seq["+valueStructure+"]"
    }
  implicit def AttributedStructure[T](implicit valueStructure: RawStructure[T]) =
    new RawStructure[sbt.Attributed[T]] {
      override def apply(t: sbt.Attributed[T]): Map[String, Any] =
        Map("attributed" -> true,
            "data" -> valueStructure.apply(t.data))
      override def unapply(map: Map[String, Any]): Option[sbt.Attributed[T]] =
        if(map("attributed") == true && map.contains("data")) {
          val data =
            map("data").asInstanceOf[Map[String,Any]]
          // TODO - Reify the attributes as well.
          valueStructure.unapply(data).map(sbt.Attributed.blank)
        } else None
      override def toString = "RawStructure[Attributed["+valueStructure+"]"
    }
}
object JsonStructure {
  def apply[T](t: T)(implicit real: RawStructure[T]): Map[String, Any] =
    real(t)
  def unapply[T](map: Map[String, Any])(implicit real: RawStructure[T]): Option[T] =
    real.unapply(map)  
}