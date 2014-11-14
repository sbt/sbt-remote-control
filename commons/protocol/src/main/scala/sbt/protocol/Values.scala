package sbt.protocol

import play.api.libs.json._
import scala.util.{ Try, Success, Failure }
/**
 *  Represents a serialized value with a stringValue fallback.
 */
final case class BuildValue(serialized: JsValue, stringValue: String) {
  def value[T](implicit reads: Reads[T]): Option[T] =
    reads.reads(serialized).asOpt
  override def equals(o: Any): Boolean =
    o match {
      case x: BuildValue => x.serialized == serialized
      case _ => false
    }
  override def hashCode: Int = serialized.hashCode
}

object BuildValue {
  def apply[T](value: T)(implicit writes: Writes[T]): BuildValue = {
    BuildValue(serialized = Json.toJson(value), stringValue = value.toString)
  }

  // we have to hand-code these due to the apply() overload which confuses Play
  implicit object buildValueReads extends Reads[BuildValue] {
    def reads(map: JsValue): JsResult[BuildValue] = {
      for {
        stringValue <- (map \ "stringValue").validate[String]
        serialized <- (map \ "serialized").validate[JsValue]
      } yield BuildValue(serialized, stringValue)
    }
  }

  implicit object buildValueWrites extends Writes[BuildValue] {
    override def writes(t: BuildValue): JsValue = {
      Json.obj("serialized" -> t.serialized, "stringValue" -> t.stringValue)
    }
  }
}

object ThrowableDeserializers {
  val empty: ThrowableDeserializers = ThrowableDeserializers()
  private[protocol]type TypePair[T] = (Manifest[T], Reads[T])
  private[protocol] def toPair[T](implicit mf: Manifest[T], reader: Reads[T]): TypePair[T] = (mf, reader)
}

final case class ThrowableDeserializers(readers: Map[Manifest[_], Reads[_]] = Map.empty[Manifest[_], Reads[_]]) {
  import ThrowableDeserializers._
  def add[T](implicit mf: Manifest[T], reader: Reads[T]): ThrowableDeserializers =
    ThrowableDeserializers(this.readers + toPair[T](mf, reader))

  def tryAnyReader(in: JsValue): Option[Throwable] =
    readers.foldLeft[Option[Throwable]](None) {
      case (None, (_, reader)) => reader.reads(in).asOpt.asInstanceOf[Option[Throwable]]
      case (x, _) => x
    }
}

/**
 * Represents the outcome of a task. The outcome can be a value or an exception.
 */
sealed trait TaskResult {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean
  final def result[A](implicit readResult: Reads[A]): Try[A] =
    resultWithCustomThrowable[A, Throwable](readResult, sbt.GenericSerializers.throwableReads)
  def resultWithCustomThrowable[A, B <: Throwable](implicit readResult: Reads[A], readFailure: Reads[B]): Try[A]
  def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit readResult: Reads[A]): Try[A]
}
/** This represents that the task was run successfully. */
final case class TaskSuccess(value: BuildValue) extends TaskResult {
  override def isSuccess = true
  override def resultWithCustomThrowable[A, B <: Throwable](implicit readResult: Reads[A], readFailure: Reads[B]): Try[A] = {
    value.value[A] match {
      case Some(v) => Success(v)
      case None => Failure(new Exception(s"Failed to deserialize ${value.serialized}"))
    }
  }
  override def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit readResult: Reads[A]): Try[A] = {
    value.value[A] match {
      case Some(v) => Success(v)
      case None => Failure(new Exception(s"Failed to deserialize ${value.serialized}"))
    }
  }
}

final case class TaskFailure(cause: BuildValue) extends TaskResult {
  override def isSuccess = false
  override def resultWithCustomThrowable[A, B <: Throwable](implicit readResult: Reads[A], readFailure: Reads[B]): Try[A] = {
    val t = readFailure.reads(cause.serialized).asOpt.getOrElse(new Exception(cause.stringValue))
    Failure(t)
  }
  override def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit readResult: Reads[A]): Try[A] = {
    val t = throwableDeserializers.tryAnyReader(cause.serialized).getOrElse(new Exception(cause.stringValue))
    Failure(t)
  }
}

object TaskResult {
  implicit val reads: Reads[TaskResult] =
    new Reads[TaskResult] {
      override def reads(m: JsValue): JsResult[TaskResult] = {
        (m \ "success") match {
          case JsBoolean(true) =>
            Json.fromJson[BuildValue](m).map(TaskSuccess.apply)
          case JsBoolean(false) =>
            Json.fromJson[BuildValue](m \ "cause").map(TaskFailure.apply)
          case _ =>
            JsError("Unable to deserialize task result.")
        }
      }
    }
  implicit val writes: Writes[TaskResult] =
    new Writes[TaskResult] {
      override def writes(t: TaskResult): JsValue =
        t match {
          case TaskFailure(cause) =>
            JsObject(Seq("success" -> JsBoolean(false), "cause" -> Json.toJson(cause)))
          case TaskSuccess(value) =>
            val base = Json.obj("success" -> true)
            val valueJson = Json.toJson(value) match {
              case o: JsObject => o
              case other => throw new RuntimeException("serialized a BuildValue to non-JsObject")
            }
            base ++ valueJson
        }
    }
}
