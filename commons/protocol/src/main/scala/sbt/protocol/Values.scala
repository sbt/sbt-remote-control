package sbt.protocol

import sbt.serialization._
import scala.util.{ Try, Success, Failure }
import scala.pickling.{ SPickler, Unpickler }

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
    BuildValue(serialized = SerializedValue(value)(pickler), stringValue = value.toString)
  }

  implicit val pickler: SPickler[BuildValue] = SPickler.genPickler[BuildValue]
  implicit val unpickler: Unpickler[BuildValue] = Unpickler.genUnpickler[BuildValue]
}

object ThrowableDeserializers {
  val empty: ThrowableDeserializers = ThrowableDeserializers()
  private[protocol]type TypePair[T] = (Manifest[T], SbtUnpickler[T])
  private[protocol] def toPair[T](implicit mf: Manifest[T], reader: SbtUnpickler[T]): TypePair[T] = (mf, reader)
}

final case class ThrowableDeserializers(readers: Map[Manifest[_], SbtUnpickler[_]] = Map.empty[Manifest[_], SbtUnpickler[_]]) {
  import ThrowableDeserializers._
  def add[T](implicit mf: Manifest[T], reader: SbtUnpickler[T]): ThrowableDeserializers =
    ThrowableDeserializers(this.readers + toPair[T](mf, reader))

  def tryAnyReader(in: SerializedValue): Option[Throwable] =
    readers.foldLeft[Option[Throwable]](None) {
      case (None, (_, reader)) => in.parse(reader).map(_.asInstanceOf[Throwable])
      case (x, _) => x
    }
}

/**
 * Represents the outcome of a task. The outcome can be a value or an exception.
 */
sealed trait TaskResult {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean

  final def result[A](implicit unpickleResult: SbtUnpickler[A]): Try[A] =
    resultWithCustomThrowable[A, Throwable](unpickleResult, implicitly[SbtUnpickler[Throwable]])
  def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: SbtUnpickler[A], unpickleFailure: SbtUnpickler[B]): Try[A]
  def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: SbtUnpickler[A]): Try[A]
}

object TaskResult {
  implicit val pickler: SPickler[TaskResult] = SPickler.genPickler[TaskResult]
  implicit val unpickler: Unpickler[TaskResult] = Unpickler.genUnpickler[TaskResult]
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
  override def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: SbtUnpickler[A]): Try[A] = {
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
  override def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: SbtUnpickler[A]): Try[A] = {
    val t = throwableDeserializers.tryAnyReader(cause.serialized).getOrElse(new Exception(cause.stringValue))
    Failure(t)
  }
}
