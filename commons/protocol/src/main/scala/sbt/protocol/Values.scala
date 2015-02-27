package sbt.protocol

import sbt.serialization._
import scala.util.{ Try, Success, Failure }

/**
 *  Represents a serialized value with a stringValue fallback.
 */
final case class BuildValue(serialized: SerializedValue, stringValue: String) {
  def value[T](implicit unpickler: Unpickler[T]): Try[T] =
    serialized.parse[T]
  override def equals(o: Any): Boolean =
    o match {
      case x: BuildValue => x.serialized == serialized
      case _ => false
    }
  override def hashCode: Int = serialized.hashCode
}

object BuildValue {
  def apply[T](value: T)(implicit pickler: Pickler[T]): BuildValue = {
    BuildValue(serialized = SerializedValue(value)(pickler), stringValue = value.toString)
  }

  implicit val pickler: Pickler[BuildValue] = genPickler[BuildValue]
  implicit val unpickler: Unpickler[BuildValue] = genUnpickler[BuildValue]
}

object ThrowableDeserializers {
  val empty: ThrowableDeserializers = ThrowableDeserializers()
  private[protocol]type TypePair[T] = (Manifest[T], Unpickler[T])
  private[protocol] def toPair[T](implicit mf: Manifest[T], reader: Unpickler[T]): TypePair[T] = (mf, reader)
}

final case class ThrowableDeserializers(readers: Map[Manifest[_], Unpickler[_]] = Map.empty[Manifest[_], Unpickler[_]]) {
  import ThrowableDeserializers._
  def add[T](implicit mf: Manifest[T], reader: Unpickler[T]): ThrowableDeserializers =
    ThrowableDeserializers(this.readers + toPair[T](mf, reader))

  def tryAnyReader(in: SerializedValue): Option[Throwable] =
    readers.foldLeft[Option[Throwable]](None) {
      case (None, (_, reader)) => in.parse(reader).toOption.map(_.asInstanceOf[Throwable])
      case (x, _) => x
    }
}

/**
 * Represents the outcome of a task. The outcome can be a value or an exception.
 */
sealed trait TaskResult {

  /** Returns whether or not a task was executed successfully. */
  def isSuccess: Boolean

  final def result[A](implicit unpickleResult: Unpickler[A]): Try[A] =
    resultWithCustomThrowable[A, Throwable](unpickleResult, implicitly[Unpickler[Throwable]])
  def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: Unpickler[A], unpickleFailure: Unpickler[B]): Try[A]
  def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: Unpickler[A]): Try[A]
}

/** This represents that the task was run successfully. */
final case class TaskSuccess(value: BuildValue) extends TaskResult {
  override def isSuccess = true
  override def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: Unpickler[A], unpickleFailure: Unpickler[B]): Try[A] =
    value.value[A]
  override def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: Unpickler[A]): Try[A] =
    value.value[A]
}

final case class TaskFailure(cause: BuildValue) extends TaskResult {
  override def isSuccess = false
  override def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: Unpickler[A], unpickleFailure: Unpickler[B]): Try[A] = {
    val t = cause.serialized.parse[B].getOrElse(new Exception(cause.stringValue))
    Failure(t)
  }
  override def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: Unpickler[A]): Try[A] = {
    val t = throwableDeserializers.tryAnyReader(cause.serialized).getOrElse(new Exception(cause.stringValue))
    Failure(t)
  }
}

object TaskSuccess {
  implicit val pickler: Pickler[TaskSuccess] = genPickler[TaskSuccess]
  implicit val unpickler: Unpickler[TaskSuccess] = genUnpickler[TaskSuccess]
}

object TaskFailure {
  implicit val pickler: Pickler[TaskFailure] = genPickler[TaskFailure]
  implicit val unpickler: Unpickler[TaskFailure] = genUnpickler[TaskFailure]
}

// TODO currently due to a pickling bug caused by a Scala bug,
// the macros won't know all the subtypes of TaskResult if we
// put this companion object earlier in the file.
object TaskResult {
  implicit val pickler: Pickler[TaskResult] = genPickler[TaskResult]
  implicit val unpickler: Unpickler[TaskResult] = genUnpickler[TaskResult]
}
