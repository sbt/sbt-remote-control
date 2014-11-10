package sbt.protocol

import sbt.serialization._
import scala.util.{ Try, Success, Failure }

/**
 *  Represents a serialized value with a stringValue fallback.
 */
final case class BuildValue(serialized: SerializedValue, stringValue: String) {
  def value[T](implicit unpickler: SbtUnpickler[T]): Option[T] =
    serialized.parse[T]
  override def equals(o: Any): Boolean =
    o match {
      case x: BuildValue => x.serialized == serialized
      case _ => false
    }
  override def hashCode: Int = serialized.hashCode
}

object BuildValue {
  def apply[T](value: T)(implicit pickler: SbtPickler[T]): BuildValue = {
    BuildValue(serialized = SerializedValue(value), stringValue = value.toString)
  }
}

/**
 * Represents the outcome of a task. The outcome can be a value or an exception.
 */
sealed trait TaskResult {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean
  final def result[A](implicit unpickleResult: SbtUnpickler[A]): Try[A] =
    resultWithCustomThrowable[A, Throwable](unpickleResult, sbtUnpicklerFromUnpickler(throwableUnpickler))
  def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: SbtUnpickler[A], unpickleFailure: SbtUnpickler[B]): Try[A]
}
/** This represents that the task was run successfully. */
final case class TaskSuccess(value: BuildValue) extends TaskResult {
  override def isSuccess = true
  override def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: SbtUnpickler[A], unpickleFailure: SbtUnpickler[B]): Try[A] = {
    value.value[A] match {
      case Some(v) => Success(v)
      case None => Failure(new Exception(s"Failed to deserialize ${value.serialized}"))
    }
  }
}

final case class TaskFailure(cause: BuildValue) extends TaskResult {
  override def isSuccess = false
  override def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: SbtUnpickler[A], unpickleFailure: SbtUnpickler[B]): Try[A] = {
    val t = cause.serialized.parse[B].getOrElse(new Exception(cause.stringValue))
    Failure(t)
  }
}
