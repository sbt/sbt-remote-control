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
  
  
  implicit object PositionStructure extends RawStructure[xsbti.Position] {
    override def apply(in: xsbti.Position): Map[String, Any] = {
      def defineIf[T](value: xsbti.Maybe[T], name: String): Seq[(String, T)] =
        if(value.isDefined) Seq(name -> value.get) else Nil
      val line = defineIf(in.line, "line")
      val offset = defineIf(in.offset, "offset")
      val pointer = defineIf(in.pointer, "pointer")
      val pointerSpace = defineIf(in.pointerSpace, "pointerSpace")
      val sourcePath = defineIf(in.sourcePath, "sourcePath")
      val sourceFile = defineIf(in.sourceFile, "sourceFile") map {
        case (name, file) => name -> FileStructure(file)
      }
      Map("lineContent" -> in.lineContent) ++ 
        line ++ 
        offset ++ 
        pointer ++ 
        pointerSpace ++
        sourcePath ++
        sourceFile
    }
    private def convert[T](o: Option[T]): xsbti.Maybe[T] = 
      o match {
        case Some(value) => xsbti.Maybe.just(value)
        case None => xsbti.Maybe.nothing()
      }
    private class PositionDeserialized(
      override val lineContent: String,
      l: Option[Int],
      o: Option[Int],
      p: Option[Int],
      ps: Option[String],
      sp: Option[String],
      sf: Option[java.io.File]
    ) extends xsbti.Position {
      override def line = convert(l.map(Integer.valueOf))
      override def offset = convert(o.map(Integer.valueOf))
      override def pointer = convert(p.map(Integer.valueOf))
      override def pointerSpace = convert(ps)
      override def sourcePath = convert(sp)
      override def sourceFile = convert(sf)
    }
    override def unapply(in: Map[String, Any]): Option[xsbti.Position] = {
      in get "lineContent" match {
        case Some(lineContent) => 
          val line = in get "line" map (_.toString.toInt)
          val offset = in get "offset" map (_.toString.toInt)
          val pointer = in get "pointer" map (_.toString.toInt)
          val pointerSpace = in get "pointerSpace" map (_.toString)
          val sourcePath = in get "sourcePath" map (_.toString)
          val sourceFile = 
            for {
              fObj <- in get "sourceFile"
              file <- FileStructure.unapply(fObj.asInstanceOf[Map[String,Any]])
            } yield file
          Some(new PositionDeserialized(lineContent.toString, line, offset, pointer, pointerSpace, sourcePath, sourceFile))
        case None => None
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