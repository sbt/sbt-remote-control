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

// TODO TaskResult does not need type parameters, get rid of them
/**
 * Represents the outcome of a task. The outcome can be a type T or an exception with type E.
 */
sealed trait TaskResult[+T, +E <: Throwable] {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean
  // this is certainly goofy right now with the T type parameter above.
  // TODO remove type parameters from the class
  final def result[A](implicit readResult: Reads[A]): Try[A] =
    resultWithCustomThrowable[A, Throwable](readResult, sbt.GenericSerializers.throwableReads)
  def resultWithCustomThrowable[A, B <: Throwable](implicit readResult: Reads[A], readFailure: Reads[B]): Try[A]
}
/** This represents that the task was run successfully. */
final case class TaskSuccess[+T, +E <: Throwable](value: BuildValue) extends TaskResult[T, E] {
  override def isSuccess = true
  override def resultWithCustomThrowable[A, B <: Throwable](implicit readResult: Reads[A], readFailure: Reads[B]): Try[A] = {
    value.value[A] match {
      case Some(v) => Success(v)
      case None => Failure(new Exception(s"Failed to deserialize ${value.serialized}"))
    }
  }
}
/** This represents that there was an error running a task, and returns the error message. */
final case class TaskFailure[+T, +E <: Throwable](message: String, cause: BuildValue) extends TaskResult[T, E] {
  override def isSuccess = false
  override def resultWithCustomThrowable[A, B <: Throwable](implicit readResult: Reads[A], readFailure: Reads[B]): Try[A] = {
    val t = readFailure.reads(cause.serialized).asOpt.getOrElse(new Exception(message))
    Failure(t)
  }
}

object TaskResult {
  implicit def reads[T, E <: Throwable]: Reads[TaskResult[T, E]] =
    new Reads[TaskResult[T, E]] {
      override def reads(m: JsValue): JsResult[TaskResult[T, E]] = {
        (m \ "success") match {
          case JsBoolean(true) =>
            Json.fromJson[BuildValue](m).map(TaskSuccess.apply)
          case JsBoolean(false) =>
            for {
              cause <- Json.fromJson[BuildValue](m \ "cause")
              message <- (m \ "message").validate[String]
            } yield TaskFailure(message, cause)
          case _ =>
            JsError("Unable to deserialize task result.")
        }
      }
    }
  implicit def writes[T, E <: Throwable]: Writes[TaskResult[T, E]] =
    new Writes[TaskResult[T, E]] {
      override def writes(t: TaskResult[T, E]): JsValue =
        t match {
          case TaskFailure(msg, cause) =>
            JsObject(Seq("success" -> JsBoolean(false), "message" -> JsString(msg), "cause" -> Json.toJson(cause)))
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
