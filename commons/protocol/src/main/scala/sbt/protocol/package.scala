package sbt

import scala.pickling.{ PReader, FastTypeTag }
import scala.util.control.NonFatal

import sbt.serialization._
import sbt.serialization.functions._

package object protocol {
  import CoreProtocol._
  import sbt.serialization.CanToString
  import scala.pickling.{ SPickler, Unpickler }

  //implicit def attributedPickler[T](implicit pickler: SPickler[T]): SPickler[Attributed[T]] = ???
  //implicit def attributedUnpickler[T](implicit unpickler: Unpickler[T]): Unpickler[Attributed[T]] = ???

  private implicit val severityCanToString: CanToString[xsbti.Severity] = CanToString(
    _.toString, {
      case "Info" => xsbti.Severity.Info
      case "Warn" => xsbti.Severity.Warn
      case "Error" => xsbti.Severity.Error
    })

  implicit val severityPickler: SPickler[xsbti.Severity] with Unpickler[xsbti.Severity] =
    canToStringPickler[xsbti.Severity]

  private def convert[T](o: Option[T]): xsbti.Maybe[T] =
    o match {
      case Some(value) => xsbti.Maybe.just(value)
      case None => xsbti.Maybe.nothing()
    }

  private def convertToOption[T](o: xsbti.Maybe[T]): Option[T] =
    if (o.isDefined()) Some(o.get())
    else None

  implicit def fileMapUnpickler[T](implicit tUnpickler: Unpickler[T]): Unpickler[Map[java.io.File, T]] = ??? /* Unpickler[Map[java.io.File, T]] { json =>
    val stringMapUnpickler = implicitly[Unpickler[Map[String, T]]]
    stringMapUnpickler.reads(json) flatMap { stringMap =>
      try JsSuccess(stringMap.map(kv => fileFromString(kv._1).getOrElse(throw new Exception(s"invalid filename ${kv._1}")) -> kv._2))
      catch {
        case e: Exception => JsError(e.getMessage)
      }
    }
  }*/

  implicit def fileMapSPickler[T](implicit tPickler: SPickler[T]): SPickler[Map[java.io.File, T]] = ??? /* SPickler[Map[java.io.File, T]] { m =>
    val stringMapPickler = implicitly[SPickler[Map[String, T]]]
    val stringMap = m.map(kv => fileToString(kv._1) -> kv._2)
    stringMapSPickler.writes(stringMap)
  } */

  implicit def relationUnpickler[A, B](implicit forwardUnpickler: Unpickler[Map[A, Set[B]]], reverseUnpickler: Unpickler[Map[B, Set[A]]]): Unpickler[Relation[A, B]] = ??? /* Unpickler[Relation[A, B]] { json =>
    ((__ \ "forwardMap").read[Map[A, Set[B]]] and
      (__ \ "reverseMap").read[Map[B, Set[A]]]).apply(Relation(_, _)).reads(json)
  }*/
  implicit def relationSPickler[A, B](implicit forwardPickler: SPickler[Map[A, Set[B]]], reversePickler: SPickler[Map[B, Set[A]]]): SPickler[Relation[A, B]] = ??? /* SPickler[Relation[A, B]] { in =>
    Json.obj("forwardMap" -> in.forwardMap, "reverseMap" -> in.reverseMap)
  }*/

  // TODO these are defs just so we don't run ??? for now
  // TODO we probably want to drop all/most Analysis-related stuff
  implicit def stampUnpickler: Unpickler[Stamp] = ???
  implicit def stampPickler: SPickler[Stamp] = ???

  implicit def qualifierUnpickler: Unpickler[Qualifier] = ???
  implicit def qualifierPickler: SPickler[Qualifier] = ???

  implicit def accessPickler: SPickler[Access] = ???
  implicit def accessUnpickler: Unpickler[Access] = ???

  implicit def variancePickler: SPickler[xsbti.api.Variance] = ???
  implicit def varianceUnpickler: Unpickler[xsbti.api.Variance] = ???
  // lazy needed to avoid NPE
  implicit lazy val pathComponentPickler: SPickler[PathComponent] = ???
  implicit lazy val pathComponentUnpickler: Unpickler[PathComponent] = ???

  // lazy needed to avoid NPE
  implicit lazy val simpleTypeUnpickler: Unpickler[SimpleType] = ???
  // This one causes ambiguity with SPickler[Type] and isn't needed as a public implicit
  // because SPickler[Type] works fine.
  private lazy val simpleTypePickler: SPickler[SimpleType] = ???
  // lazy needed to avoid NPE
  implicit lazy val typeUnpickler: Unpickler[Type] = ???
  // lazy needed to avoid NPE
  implicit lazy val typePickler: SPickler[Type] = ???

  // TODO there's no real logic to why these are here and others are in
  // companion objects.
  implicit val positionPickler = genPickler[Position]
  implicit val positionUnpickler = genUnpickler[Position]
  implicit val compilationFailurePickler = genPickler[CompilationFailure]
  implicit val compilationFailureUnpickler = genUnpickler[CompilationFailure]
  implicit val moduleIdPickler = genPickler[ModuleId]
  implicit object moduleIdUnpickler extends Unpickler[ModuleId] {
    private val stringUnpickler = CoreProtocol.stringPickler
    private val attrsUnpickler = CoreProtocol.stringMapPickler[String]
    override def unpickle(tag: String, reader: PReader): Any = {
      reader.pushHints()
      reader.hintTag(this.tag)
      reader.hintStaticallyElidedType()
      // TODO - this was already called...
      reader.beginEntry()
      def readString(field: String): String = {
        stringUnpickler.unpickleEntry(reader.readField(field)).toString
      }
      val org = readString("organization")
      val n = readString("name")
      val attrs = {
        val sr = reader.readField("attributes")
        sr.hintTag(attrsUnpickler.tag)
        sr.hintStaticallyElidedType()
        attrsUnpickler.unpickle(attrsUnpickler.tag.key, sr).asInstanceOf[Map[String, String]]
      }
      reader.endEntry()
      reader.popHints()
      ModuleId(org, n, attrs)
    }
    override val tag: FastTypeTag[ModuleId] = implicitly[FastTypeTag[ModuleId]]
  }
}
