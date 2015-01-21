package sbt.serialization
package pickler

import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }

trait PrimitivePicklers {
  implicit val bytePickler: SPickler[Byte] with Unpickler[Byte] = PrimitivePickler[Byte]
  implicit val shortPickler: SPickler[Short] with Unpickler[Short] = PrimitivePickler[Short]
  implicit val charPickler: SPickler[Char] with Unpickler[Char] = PrimitivePickler[Char]
  implicit val intPickler: SPickler[Int] with Unpickler[Int] = PrimitivePickler[Int]
  implicit val longPickler: SPickler[Long] with Unpickler[Long] = PrimitivePickler[Long]
  implicit val booleanPickler: SPickler[Boolean] with Unpickler[Boolean] = PrimitivePickler[Boolean]
  implicit val floatPickler: SPickler[Float] with Unpickler[Float] = PrimitivePickler[Float]
  implicit val doublePickler: SPickler[Double] with Unpickler[Double] = PrimitivePickler[Double]
  implicit val nullPickler: SPickler[Null] with Unpickler[Null] = PrimitivePickler[Null]
  implicit val stringPickler: SPickler[String] with Unpickler[String] = PrimitivePickler[String]
  implicit val unitPickler: SPickler[Unit] with Unpickler[Unit] = PrimitivePickler[Unit]
}

trait PrimitiveArrayPicklers {
  implicit val byteArrayPickler: SPickler[Array[Byte]] with Unpickler[Array[Byte]] = PrimitivePickler[Array[Byte]]
  implicit val shortArrayPickler: SPickler[Array[Short]] with Unpickler[Array[Short]] = PrimitivePickler[Array[Short]]
  implicit val charArrayPickler: SPickler[Array[Char]] with Unpickler[Array[Char]] = PrimitivePickler[Array[Char]]
  implicit val intArrayPickler: SPickler[Array[Int]] with Unpickler[Array[Int]] = PrimitivePickler[Array[Int]]
  implicit val longArrayPickler: SPickler[Array[Long]] with Unpickler[Array[Long]] = PrimitivePickler[Array[Long]]
  implicit val booleanArrayPickler: SPickler[Array[Boolean]] with Unpickler[Array[Boolean]] = PrimitivePickler[Array[Boolean]]
  implicit val floatArrayPickler: SPickler[Array[Float]] with Unpickler[Array[Float]] = PrimitivePickler[Array[Float]]
  implicit val doubleArrayPickler: SPickler[Array[Double]] with Unpickler[Array[Double]] = PrimitivePickler[Array[Double]]
}

// this isn't exactly cut-and-paste because we remove the runtime pickler registration
private[serialization] class PrimitivePickler[T: FastTypeTag](name: String) extends SPickler[T] with Unpickler[T] {
  override def tag: FastTypeTag[T] = implicitly[FastTypeTag[T]]
  override def pickle(picklee: T, builder: PBuilder): Unit = {
    builder.hintTag(tag)
    builder.hintStaticallyElidedType()
    builder.beginEntry(picklee)
    builder.endEntry()
  }
  override def unpickle(tag: String, reader: PReader): Any = {
    try {
      val result = reader.readPrimitive()
      result
    } catch {
      case PicklingException(msg, cause) =>
        throw PicklingException(s"""error in unpickle of primitive unpickler '$name':
                                  |tag in unpickle: '${tag}'
                                  |message:
                                  |  $msg""".stripMargin, cause)
    }
  }
}
private[serialization] object PrimitivePickler {
  def apply[A: FastTypeTag]: SPickler[A] with Unpickler[A] =
    new PrimitivePickler[A](FastTypeTag.valueTypeName(implicitly[FastTypeTag[A]]))
}
