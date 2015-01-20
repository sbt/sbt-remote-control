package sbt.protocol

import sbt.serialization._
import scala.util.{ Try, Success, Failure }
import scala.pickling.{ SPickler, Unpickler, allPicklers }
// TODO - needed for genPickler
import scala.pickling.ops._
/**
 *  Represents a serialized value with a stringValue fallback.
 */
final case class BuildValue(serialized: SerializedValue, stringValue: String) {
  def value[T](implicit unpickler: Unpickler[T]): Option[T] =
    serialized.parse[T].toOption
  override def equals(o: Any): Boolean =
    o match {
      case x: BuildValue => x.serialized == serialized
      case _ => false
    }
  override def hashCode: Int = serialized.hashCode
}

object BuildValue {
  def apply[T](value: T)(implicit pickler: SPickler[T]): BuildValue = {
    BuildValue(serialized = SerializedValue(value)(pickler), stringValue = value.toString)
  }

  implicit val pickler: SPickler[BuildValue] = allPicklers.genPickler[BuildValue]
  implicit val unpickler: Unpickler[BuildValue] = allPicklers.genUnpickler[BuildValue]
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
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean

  final def result[A](implicit unpickleResult: Unpickler[A]): Try[A] =
    resultWithCustomThrowable[A, Throwable](unpickleResult, implicitly[Unpickler[Throwable]])
  def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: Unpickler[A], unpickleFailure: Unpickler[B]): Try[A]
  def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: Unpickler[A]): Try[A]
}

/** This represents that the task was run successfully. */
final case class TaskSuccess(value: BuildValue) extends TaskResult {
  override def isSuccess = true
  override def resultWithCustomThrowable[A, B <: Throwable](implicit unpickleResult: Unpickler[A], unpickleFailure: Unpickler[B]): Try[A] = {
    value.value[A] match {
      case Some(v) => Success(v)
      case None => Failure(new Exception(s"Failed to deserialize ${value.serialized}"))
    }
  }
  override def resultWithCustomThrowables[A](throwableDeserializers: ThrowableDeserializers)(implicit unpickleResult: Unpickler[A]): Try[A] = {
    value.value[A] match {
      case Some(v) => Success(v)
      case None => Failure(new Exception(s"Failed to deserialize ${value.serialized}"))
    }
  }
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
  implicit val pickler: SPickler[TaskSuccess] = allPicklers.genPickler[TaskSuccess]
  implicit val unpickler: Unpickler[TaskSuccess] = allPicklers.genUnpickler[TaskSuccess]
}

object TaskFailure {
  implicit val pickler: SPickler[TaskFailure] = allPicklers.genPickler[TaskFailure]
  implicit val unpickler: Unpickler[TaskFailure] = allPicklers.genUnpickler[TaskFailure]
}

// TODO currently due to a pickling bug caused by a Scala bug,
// the macros won't know all the subtypes of TaskResult if we
// put this companion object earlier in the file.
object TaskResult {
  implicit val pickler: SPickler[TaskResult] = allPicklers.genPickler[TaskResult]
  implicit val unpickler: Unpickler[TaskResult] = allPicklers.genUnpickler[TaskResult]
}
