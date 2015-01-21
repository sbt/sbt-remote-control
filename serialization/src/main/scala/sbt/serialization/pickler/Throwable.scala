package sbt.serialization
package pickler

import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }

trait ThrowablePicklers extends PrimitivePicklers with OptionPicklers with VectorPicklers {
  private final case class StackTraceElementDeserialized(declaringClass: String,
    methodName: String,
    fileName: String,
    lineNumber: Int)

  private object StackTraceElementDeserialized {
    implicit val pickler = SPickler.generate[StackTraceElementDeserialized]
    implicit val unpickler = Unpickler.generate[StackTraceElementDeserialized]
  }

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
    private val vstedPickler = implicitly[SPickler[Vector[StackTraceElementDeserialized]]]
    private val vstedUnpickler = implicitly[Unpickler[Vector[StackTraceElementDeserialized]]]

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
      val stackTrace = vstedUnpickler.unpickleEntry(preader.readField("stackTrace")).asInstanceOf[Vector[StackTraceElementDeserialized]]
      val result = new Exception(message.orNull, cause.orNull)
      result.setStackTrace((stackTrace map { x =>
        new StackTraceElement(x.declaringClass, x.methodName, x.fileName, x.lineNumber)
      }).toArray)
      result
    }
  }
}
