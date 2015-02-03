package sbt.serialization
package pickler

// TODO - Why is alias not working.
import scala.pickling.pickler.{ PrimitivePicklers, RefPicklers }
import scala.pickling.PicklingException

trait ThrowablePicklers extends PrimitivePicklers with OptionPicklers with VectorPicklers with RefPicklers {

  private implicit object stackTracePickler extends SPicklerUnpickler[StackTraceElement] {
    override val tag: FastTypeTag[StackTraceElement] = implicitly[FastTypeTag[StackTraceElement]]
    private val intTag = implicitly[FastTypeTag[Int]]
    private val stringOptTag = implicitly[FastTypeTag[Option[String]]]
    private val stringOptPickler = implicitly[SPickler[Option[String]]]
    private val stringOptUnpickler = implicitly[Unpickler[Option[String]]]

    override def pickle(a: StackTraceElement, builder: PBuilder): Unit = {
      builder.beginEntry(a)
      def pickleString(field: String, value: String): Unit = {
        builder.putField(field, { b =>
          b.hintTag(stringOptTag)
          stringOptPickler.pickle(Option(value), b)
        })
      }
      pickleString("className", a.getClassName)
      pickleString("methodName", a.getMethodName)
      pickleString("fileName", a.getFileName)
      builder.putField("lineNumber", { b =>
        b.hintTag(intTag)
        intPickler.pickle(a.getLineNumber, b)
      })
      builder.endEntry()
    }
    override def unpickle(tag: String, preader: PReader): StackTraceElement = {
      def unpickleString(field: String): Option[String] = {
        stringOptUnpickler.unpickleEntry(preader.readField(field)).asInstanceOf[Option[String]]
      }
      val className = unpickleString("className")
      val methodName = unpickleString("methodName")
      val fileName = unpickleString("fileName")
      val lineNumber = intPickler.unpickleEntry(preader.readField("lineNumber")).asInstanceOf[Int]
      new StackTraceElement(className.orNull, methodName.orNull, fileName.orNull, lineNumber)
    }
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
    private val vsteTag = implicitly[FastTypeTag[Vector[StackTraceElement]]]
    private val vstePickler = vectorPickler[StackTraceElement]
    private val vsteUnpickler = vstePickler

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
        b.hintTag(vsteTag)
        vstePickler.pickle(a.getStackTrace.toVector, b)
      })
      builder.endEntry()
    }
    def unpickle(tag: String, preader: PReader): Any = {
      val message = stringOptUnpickler.unpickleEntry(preader.readField("message")).asInstanceOf[Option[String]]
      val cause = throwableOptPicklerUnpickler.unpickleEntry(preader.readField("cause")).asInstanceOf[Option[Throwable]]
      preader.hintStaticallyElidedType()
      val stackTrace = vsteUnpickler.unpickleEntry(preader.readField("stackTrace")).asInstanceOf[Vector[StackTraceElement]]
      val result = new Exception(message.orNull, cause.orNull)
      result.setStackTrace(stackTrace.toArray)
      result
    }
  }
}
