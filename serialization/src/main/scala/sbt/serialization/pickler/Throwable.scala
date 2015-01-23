package sbt.serialization
package pickler

import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }
import scala.pickling.static._

private[pickler] final case class StackTraceElementDeserialized(declaringClass: String,
  methodName: String,
  fileName: String,
  lineNumber: Int)

trait ThrowablePicklers extends PrimitivePicklers with OptionPicklers with VectorPicklers with RefPicklers {

  implicit val stackTracePickler: SPickler[StackTraceElementDeserialized] = SPickler.generate[StackTraceElementDeserialized]
  implicit val stackTraceUnickler: Unpickler[StackTraceElementDeserialized] = Unpickler.generate[StackTraceElementDeserialized]
  implicit val pickler = SPickler.generate[StackTraceElementDeserialized]
  implicit val unpickler = Unpickler.generate[StackTraceElementDeserialized]

  // TODO why isn't this in LowPriority / what goes in Low and what goes here?
  implicit object throwablePicklerUnpickler extends SPickler[Throwable] with Unpickler[Throwable] {
    val tag: FastTypeTag[Throwable] = implicitly[FastTypeTag[Throwable]]
    private val stringTag = implicitly[FastTypeTag[String]]
    private val stringOptTag = implicitly[FastTypeTag[Option[String]]]
    private val throwableOptTag = implicitly[FastTypeTag[Option[Throwable]]]
    private val stringOptPickler = implicitly[SPickler[Option[String]]]
    private val stringOptUnpickler = implicitly[Unpickler[Option[String]]]
    private val throwableOptPicklerUnpickler = optionPickler[Throwable](tag, this, this, throwableOptTag)
    private val vstedTag = implicitly[FastTypeTag[Vector[StackTraceElementDeserialized]]]
    private val vstedPickler = vectorPickler[StackTraceElementDeserialized]

    //implicitly[SPickler[Vector[StackTraceElementDeserialized]]]
    private val vstedUnpickler = vstedPickler

    def pickle(a: Throwable, builder: PBuilder): Unit = {
      builder.beginEntry(a)
      builder.putField("message", { b =>
        b.hintTag(stringOptTag)
        stringOptPickler.pickle(Option(a.getMessage), b)
      })
      builder.putField("cause", { b =>
        b.hintTag(throwableOptTag)
        throwableOptPicklerUnpickler.pickle(Option(a.getCause), b)
      })
      builder.putField("stackTrace", { b =>
        b.hintTag(vstedTag)
        vstedPickler.pickle(a.getStackTrace.toVector map { x =>
          StackTraceElementDeserialized(x.getClassName, x.getMethodName, x.getFileName, x.getLineNumber)
        }, b)
      })
      builder.endEntry()
    }
    def unpickle(tag: String, preader: PReader): Any = {
      val message = stringOptUnpickler.unpickleEntry(preader.readField("message")).asInstanceOf[Option[String]]
      val cause = throwableOptPicklerUnpickler.unpickleEntry(preader.readField("cause")).asInstanceOf[Option[Throwable]]
      preader.hintStaticallyElidedType()
      val stackTrace = vstedUnpickler.unpickleEntry(preader.readField("stackTrace")).asInstanceOf[Vector[StackTraceElementDeserialized]]
      val result = new Exception(message.orNull, cause.orNull)
      result.setStackTrace((stackTrace map { x =>
        new StackTraceElement(x.declaringClass, x.methodName, x.fileName, x.lineNumber)
      }).toArray)
      result
    }
  }
}
