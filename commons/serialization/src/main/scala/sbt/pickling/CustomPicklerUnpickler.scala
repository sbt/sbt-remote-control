package sbt.pickling

import java.io.File
import java.net.URI
import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }
import scala.reflect.runtime.universe._
import scala.collection.immutable.::
import scala.collection.generic.CanBuildFrom
import scala.pickling.internal.AppliedType

// TODO change defs to objects and vals when possible
trait CustomPicklerUnpickler extends LowPriorityCustomPicklerUnpickler {
  import scala.language.experimental.macros

  // TODO move this to sbt.serialization once it works to do so
  private implicit def staticOnly = scala.pickling.static.StaticOnly

  // non-implicit aliases of pickling's gen macros
  def genPickler[T]: SPickler[T] = macro scala.pickling.Compat.PicklerMacros_impl[T]
  def genUnpickler[T]: Unpickler[T] with scala.pickling.Generated = macro scala.pickling.Compat.UnpicklerMacros_impl[T]

  /*  === Begin cut-and-paste of primitive picklers from pickling === */

  // this isn't exactly cut-and-paste because we remove the runtime pickler registration
  private class PrimitivePicklerUnpickler[T: FastTypeTag](name: String) extends SPickler[T] with Unpickler[T] {
    override def tag: FastTypeTag[T] = implicitly[FastTypeTag[T]]
    override def pickle(picklee: T, builder: PBuilder): Unit = {
      builder.beginEntry(picklee)
      builder.endEntry()
    }
    override def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = {
      try {
        reader.readPrimitive()
      } catch {
        case PicklingException(msg, cause) =>
          throw PicklingException(s"""error in unpickle of primitive unpickler '$name':
                                    |tag in unpickle: '${tag.key}'
                                    |message:
                                    |$msg""".stripMargin, cause)
      }
    }
  }

  private def mkPrimitivePicklerUnpickler[T: FastTypeTag]: SPickler[T] with Unpickler[T] =
    new PrimitivePicklerUnpickler[T](FastTypeTag.valueTypeName(implicitly[FastTypeTag[T]]))

  implicit val bytePicklerUnpickler: SPickler[Byte] with Unpickler[Byte] = mkPrimitivePicklerUnpickler[Byte]
  implicit val shortPicklerUnpickler: SPickler[Short] with Unpickler[Short] = mkPrimitivePicklerUnpickler[Short]
  implicit val charPicklerUnpickler: SPickler[Char] with Unpickler[Char] = mkPrimitivePicklerUnpickler[Char]
  implicit val intPicklerUnpickler: SPickler[Int] with Unpickler[Int] = mkPrimitivePicklerUnpickler[Int]
  implicit val longPicklerUnpickler: SPickler[Long] with Unpickler[Long] = mkPrimitivePicklerUnpickler[Long]
  implicit val booleanPicklerUnpickler: SPickler[Boolean] with Unpickler[Boolean] = mkPrimitivePicklerUnpickler[Boolean]
  implicit val floatPicklerUnpickler: SPickler[Float] with Unpickler[Float] = mkPrimitivePicklerUnpickler[Float]
  implicit val doublePicklerUnpickler: SPickler[Double] with Unpickler[Double] = mkPrimitivePicklerUnpickler[Double]
  implicit val nullPicklerUnpickler: SPickler[Null] with Unpickler[Null] = mkPrimitivePicklerUnpickler[Null]
  implicit val stringPicklerUnpickler: SPickler[String] with Unpickler[String] = mkPrimitivePicklerUnpickler[String]
  implicit val unitPicklerUnpickler: SPickler[Unit] with Unpickler[Unit] = mkPrimitivePicklerUnpickler[Unit]

  implicit val byteArrPicklerUnpickler: SPickler[Array[Byte]] with Unpickler[Array[Byte]] = mkPrimitivePicklerUnpickler[Array[Byte]]
  implicit val shortArrPicklerUnpickler: SPickler[Array[Short]] with Unpickler[Array[Short]] = mkPrimitivePicklerUnpickler[Array[Short]]
  implicit val charArrPicklerUnpickler: SPickler[Array[Char]] with Unpickler[Array[Char]] = mkPrimitivePicklerUnpickler[Array[Char]]
  implicit val intArrPicklerUnpickler: SPickler[Array[Int]] with Unpickler[Array[Int]] = mkPrimitivePicklerUnpickler[Array[Int]]
  implicit val longArrPicklerUnpickler: SPickler[Array[Long]] with Unpickler[Array[Long]] = mkPrimitivePicklerUnpickler[Array[Long]]
  implicit val booleanArrPicklerUnpickler: SPickler[Array[Boolean]] with Unpickler[Array[Boolean]] = mkPrimitivePicklerUnpickler[Array[Boolean]]
  implicit val floatArrPicklerUnpickler: SPickler[Array[Float]] with Unpickler[Array[Float]] = mkPrimitivePicklerUnpickler[Array[Float]]
  implicit val doubleArrPicklerUnpickler: SPickler[Array[Double]] with Unpickler[Array[Double]] = mkPrimitivePicklerUnpickler[Array[Double]]

  /*  === End cut-and-paste of primitive picklers from pickling === */

  // Guard pickler
  implicit def somePickler[A: FastTypeTag]: SPickler[Some[A]] with Unpickler[Some[A]] =
    sys.error("use the pickler for Option[A]")
  // Guard pickler
  implicit lazy val nonePickler: SPickler[None.type] with Unpickler[None.type] =
    sys.error("use the pickler for Option[A]")
  implicit def optionPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Option[A]]): SPickler[Option[A]] with Unpickler[Option[A]] = new SPickler[Option[A]] with Unpickler[Option[A]] {
    private implicit val elemTag = implicitly[FastTypeTag[A]]
    val tag = implicitly[FastTypeTag[Option[A]]]
    private val isPrimitive = elemTag.tpe.isEffectivelyPrimitive
    private val nullPickler = implicitly[SPickler[Null]]

    def pickle(coll: Option[A], builder: PBuilder): Unit = {
      builder.beginEntry(coll)
      builder.pushHints()

      coll match {
        case Some(elem) =>
          if (isPrimitive) {
            builder.hintStaticallyElidedType()
            builder.hintTag(elemTag)
            builder.pinHints()
          } else builder.hintTag(elemTag)
          elemPickler.pickle(elem, builder)
        case None =>
          builder.hintTag(FastTypeTag.Null)
          nullPickler.pickle(null, builder)
      }

      builder.popHints()
      builder.endEntry()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      val reader = preader.beginCollection()
      preader.pushHints()
      if (isPrimitive) {
        reader.hintStaticallyElidedType()
        reader.hintTag(elemTag)
        reader.pinHints()
      } else reader.hintTag(elemTag)
      val length = reader.readLength
      val result: Option[A] =
        if (length == 0) None
        else {
          val r = reader.readElement()
          r.beginEntryNoTag()
          val elem = elemUnpickler.unpickle(elemTag, r)
          r.endEntry()
          Some(elem.asInstanceOf[A])
        }
      preader.popHints()
      preader.endCollection()
      result
    }
  }

  private final case class StackTraceElementDeserialized(declaringClass: String,
    methodName: String,
    fileName: String,
    lineNumber: Int)

  private object StackTraceElementDeserialized {
    implicit val pickler = genPickler[StackTraceElementDeserialized]
    implicit val unpickler = genUnpickler[StackTraceElementDeserialized]
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
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      val reader1 = preader.readField("message")
      reader1.hintTag(stringOptTag)
      val message = stringOptUnpickler.unpickle(stringOptTag, reader1).asInstanceOf[Option[String]]
      val reader2 = preader.readField("cause")
      reader2.hintTag(throwableOptTag)
      val cause = throwableOptPicklerUnpickler.unpickle(throwableOptTag, reader2).asInstanceOf[Option[Throwable]]
      val reader3 = preader.readField("stackTrace")
      reader3.hintTag(vstedTag)
      val stackTrace = vstedUnpickler.unpickle(vstedTag, reader3).asInstanceOf[Vector[StackTraceElementDeserialized]]
      val result = new Exception(message.orNull, cause.orNull)
      result.setStackTrace((stackTrace map { x =>
        new StackTraceElement(x.declaringClass, x.methodName, x.fileName, x.lineNumber)
      }).toArray)
      result
    }
  }

  implicit def canToStringPickler[A: FastTypeTag](implicit canToString: CanToString[A]): SPickler[A] with Unpickler[A] = new SPickler[A] with Unpickler[A] {
    val tag = implicitly[FastTypeTag[A]]
    val stringPickler = implicitly[SPickler[String]]
    val stringUnpickler = implicitly[Unpickler[String]]
    def pickle(a: A, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FastTypeTag.String)
      stringPickler.pickle(canToString.toString(a), builder)
      builder.popHints()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      val s = stringUnpickler.unpickle(FastTypeTag.String, preader).asInstanceOf[String]
      try {
        val result = canToString.fromString(s)
        result
      } catch {
        case _: Throwable => throw new PicklingException(s""""$s" is not valid ${tag.tpe}""")
      }
    }
  }
  implicit val appliedTypePickler: SPickler[AppliedType] with Unpickler[AppliedType] =
    canToStringPickler[AppliedType](implicitly[FastTypeTag[AppliedType]], implicitly[CanToString[AppliedType]])
  implicit val filePickler: SPickler[File] with Unpickler[File] =
    canToStringPickler[File](implicitly[FastTypeTag[File]], implicitly[CanToString[File]])
  implicit val uriPickler: SPickler[URI] with Unpickler[URI] =
    canToStringPickler[URI](implicitly[FastTypeTag[URI]], implicitly[CanToString[URI]])

  // TODO this is sort of a weird implementation detail of pickling,
  // that will hopefully not be necessary at some point
  implicit val refUnpickler: Unpickler[scala.pickling.refs.Ref] = mkPrimitivePicklerUnpickler[scala.pickling.refs.Ref]
}

trait LowPriorityCustomPicklerUnpickler {
  // TODO move this to sbt.serialization once it works to do so
  private implicit def staticOnly = scala.pickling.static.StaticOnly

  implicit def vectorPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T], collTag: FastTypeTag[Vector[T]], cbf: CanBuildFrom[Vector[T], T, Vector[T]]): SPickler[Vector[T]] with Unpickler[Vector[T]] =
    mkSeqSetPickler[T, Vector]
  implicit def arrayPickler[A >: Null: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Array[A]], cbf: CanBuildFrom[Array[A], A, Array[A]]): SPickler[Array[A]] with Unpickler[Array[A]] =
    mkTravPickler[A, Array[A]]

  implicit def listUnpickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    collTag: FastTypeTag[List[A]]): SPickler[List[A]] with Unpickler[List[A]] =
    mkTravPickler[A, List[A]]

  // Guard pickler
  implicit def seqPickler[A: FastTypeTag]: SPickler[Seq[A]] with Unpickler[Seq[A]] =
    sys.error("use Vector[A] or List[A] instead")
  // Guard pickler
  implicit def setPickler[A: FastTypeTag]: SPickler[Set[A]] with Unpickler[Set[A]] =
    sys.error("use Vector[A] or List[A] instead")

  implicit class RichType(tpe: scala.reflect.api.Universe#Type) {
    import definitions._
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }
  }

  def mkSeqSetPickler[A: FastTypeTag, Coll[_] <: Traversable[_]](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    cbf: CanBuildFrom[Coll[A], A, Coll[A]],
    collTag: FastTypeTag[Coll[A]]): SPickler[Coll[A]] with Unpickler[Coll[A]] =
    mkTravPickler[A, Coll[A]]
  def mkTravPickler[A: FastTypeTag, C <% Traversable[_]: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], cbf: CanBuildFrom[C, A, C],
    collTag: FastTypeTag[C]): SPickler[C] with Unpickler[C] =
    new SPickler[C] with Unpickler[C] {
      private implicit val elemTag = implicitly[FastTypeTag[A]]
      private val isPrimitive = elemTag.tpe.isEffectivelyPrimitive
      val tag = collTag

      def pickle(coll: C, builder: PBuilder): Unit = {
        if (elemTag == FastTypeTag.Int) builder.hintKnownSize(coll.size * 4 + 100)
        builder.beginEntry(coll)
        builder.beginCollection(coll.size)

        builder.pushHints()
        if (isPrimitive) {
          builder.hintStaticallyElidedType()
          builder.hintTag(elemTag)
          builder.pinHints()
        }

        (coll: Traversable[_]).asInstanceOf[Traversable[A]].foreach { (elem: A) =>
          builder putElement { b =>
            if (!isPrimitive) b.hintTag(elemTag)
            elemPickler.pickle(elem, b)
          }
        }

        builder.popHints()
        builder.endCollection()
        builder.endEntry()
      }

      def unpickle(tpe: => FastTypeTag[_], preader: PReader): Any = {
        val reader = preader.beginCollection()

        preader.pushHints()
        if (isPrimitive) {
          reader.hintStaticallyElidedType()
          reader.hintTag(elemTag)
          reader.pinHints()
        } else {
          reader.hintTag(elemTag) // custom code here
          reader.pinHints() // custom code here
        }

        val length = reader.readLength()
        val builder = cbf.apply()
        var i = 0
        while (i < length) {
          val r = reader.readElement()
          r.beginEntryNoTag()
          val elem = elemUnpickler.unpickle(elemTag, r)
          r.endEntry()
          builder += elem.asInstanceOf[A]
          i = i + 1
        }

        preader.popHints()
        preader.endCollection()
        builder.result
      }
    }
}
