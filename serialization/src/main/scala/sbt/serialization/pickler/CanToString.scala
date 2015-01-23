package sbt.serialization
package pickler

import java.io.File
import java.net.URI
import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }

trait CanToStringPicklers extends PrimitivePicklers {
  implicit def canToStringPickler[A: FastTypeTag](implicit canToString: CanToString[A]): SPickler[A] with Unpickler[A] = new SPickler[A] with Unpickler[A] {
    val tag = implicitly[FastTypeTag[A]]
    def pickle(a: A, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FastTypeTag.String)
      builder.hintStaticallyElidedType()
      stringPickler.pickle(canToString.toString(a), builder)
      builder.popHints()
    }
    def unpickle(tag: String, preader: PReader): Any = {
      preader.pushHints()
      preader.hintTag(FastTypeTag.String)
      preader.hintStaticallyElidedType()
      preader.pinHints()
      val s = stringPickler.unpickle(FastTypeTag.String.key, preader).asInstanceOf[String]
      preader.unpinHints()
      preader.popHints()
      try {
        val result = canToString.fromString(s)
        result
      } catch {
        case e: PicklingException => throw e
        case e: Throwable => throw PicklingException(s""""$s" is not valid ${tag}""", Some(e))
      }
    }
  }
  implicit val typeExpressionPickler: SPickler[TypeExpression] with Unpickler[TypeExpression] =
    canToStringPickler[TypeExpression](implicitly[FastTypeTag[TypeExpression]], implicitly[CanToString[TypeExpression]])
  implicit val filePickler: SPickler[File] with Unpickler[File] =
    canToStringPickler[File](implicitly[FastTypeTag[File]], implicitly[CanToString[File]])
  implicit val uriPickler: SPickler[URI] with Unpickler[URI] =
    canToStringPickler[URI](implicitly[FastTypeTag[URI]], implicitly[CanToString[URI]])
}