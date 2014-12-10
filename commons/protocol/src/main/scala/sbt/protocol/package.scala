package sbt

import scala.util.control.NonFatal
import sbt.protocol.TestOutcome

package object protocol {
  import sbt.serialization._
  import sbt.pickling.CanToString
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

  implicit val testOutcomeUnpickler: Unpickler[TestOutcome] = ???

  implicit val testOutcomePickler: SPickler[TestOutcome] = ???

  implicit val testGroupResultUnpickler: Unpickler[TestGroupResult] = ???

  implicit val testGroupResultPickler: SPickler[TestGroupResult] = ???

  implicit val immutableByteArrayUnpickler: Unpickler[ByteArray] = ???
  implicit val immutableByteArrayPickler: SPickler[ByteArray] = ???

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

  implicit val stampUnpickler: Unpickler[Stamp] = ???
  implicit val stampPickler: SPickler[Stamp] = ???

  implicit val qualifierUnpickler: Unpickler[Qualifier] = ???
  implicit val qualifierPickler: SPickler[Qualifier] = ???

  implicit val accessPickler: SPickler[Access] = ???
  implicit val accessUnpickler: Unpickler[Access] = ???

  implicit val variancePickler: SPickler[xsbti.api.Variance] = ???
  implicit val varianceUnpickler: Unpickler[xsbti.api.Variance] = ???
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

  implicit val compileFailedExceptionUnpickler: Unpickler[CompileFailedException] = ???
  implicit val compileFailedExceptionPickler: SPickler[CompileFailedException] = ???
}
