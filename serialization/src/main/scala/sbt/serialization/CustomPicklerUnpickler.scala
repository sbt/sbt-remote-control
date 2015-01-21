package sbt.serialization

import java.io.File
import java.net.URI
import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }
import scala.reflect.runtime.universe._
import scala.collection.immutable.::
import scala.collection.generic.CanBuildFrom
import scala.pickling.internal.AppliedType

// Needed for genPickler for now.
import scala.pickling.ops._

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

  implicit def optionPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Option[A]]): SPickler[Option[A]] with Unpickler[Option[A]] = new SPickler[Option[A]] with Unpickler[Option[A]] {
    private implicit val elemTag = implicitly[FastTypeTag[A]]
    val tag = implicitly[FastTypeTag[Option[A]]]
    private val isPrimitive = elemTag.tpe.isEffectivelyPrimitive
    private val nullPickler = implicitly[SPickler[Null]]
    private val nullTag = implicitly[FastTypeTag[Null]]
    def pickle(coll: Option[A], builder: PBuilder): Unit = {
      // Here we cheat the "entry" so that the notion of option
      // is erased for "null"
      coll match {
        case Some(elem) =>
          builder.hintTag(tag)
          builder.beginEntry(coll)
          builder.beginCollection(1)
          builder.putElement { b =>
            b.hintTag(elemTag)
            b.hintStaticallyElidedType()
            elemPickler.pickle(elem, b)
          }
          builder.endCollection()
          builder.endEntry()
        case None =>
          // TODO - Json Format shoudl special case this.
          builder.hintTag(tag)
          builder.beginEntry(None)
          builder.beginCollection(0)
          builder.endCollection()
          builder.endEntry()
      }
    }
    def unpickle(tag: String, preader: PReader): Any = {
      // Note - if we call beginEntry we should see JNothing or JNull show up if the option is empty.
      val reader = preader.beginCollection()
      preader.pushHints()
      // TODO - we may be ALWAYS eliding the type, so we shouldn't use an isPrimitive hack here.
      if (isPrimitive) {
        reader.hintStaticallyElidedType()
        reader.hintTag(elemTag)
        reader.pinHints()
      } else reader.hintTag(elemTag)
      val length = reader.readLength
      val result: Option[A] =
        if (length == 0) None
        else {
          val elem = elemUnpickler.unpickleEntry(reader.readElement())
          Some(elem.asInstanceOf[A])
        }
      if (isPrimitive) preader.unpinHints()
      preader.popHints()
      reader.endCollection()
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

  implicit def canToStringPickler[A: FastTypeTag](implicit canToString: CanToString[A]): SPickler[A] with Unpickler[A] = new SPickler[A] with Unpickler[A] {
    val tag = implicitly[FastTypeTag[A]]
    val stringPickler = implicitly[SPickler[String]]
    val stringUnpickler = implicitly[Unpickler[String]]
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
      val s = stringUnpickler.unpickle(FastTypeTag.String.key, preader).asInstanceOf[String]
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

  // FIXME this could theoretically work for M<:Map[String,A] and use a CanBuildFrom for M?
  implicit def stringMapPickler[A](implicit valuePickler: SPickler[A], valueUnpickler: Unpickler[A], valueTag: FastTypeTag[A],
    mapTag: FastTypeTag[Map[String, A]],
    keysPickler: SPickler[List[String]], keysUnpickler: Unpickler[List[String]]): SPickler[Map[String, A]] with Unpickler[Map[String, A]] = new SPickler[Map[String, A]] with Unpickler[Map[String, A]] {
    override val tag = mapTag

    def pickle(m: Map[String, A], builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(mapTag)
      builder.hintStaticallyElidedType()
      builder.beginEntry(m)
      // This is a pseudo-field that the JSON format will ignore reading, but
      // the binary format WILL write.
      // TODO - We should have this be a "hintDynamicKeys" instead.
      builder.putField("$keys", { b =>
        keysPickler.pickle(m.keys.toList.sorted, b)
      })
      m foreach { kv =>
        builder.putField(kv._1, { b =>
          b.hintTag(valueTag)
          valuePickler.pickle(kv._2, b)
        })
      }
      builder.endEntry()
      builder.popHints()
    }

    def unpickle(tpe: String, reader: PReader): Any = {
      reader.pushHints()
      reader.hintStaticallyElidedType()
      reader.hintTag(mapTag)
      reader.hintStaticallyElidedType()
      reader.beginEntry()
      val keys = keysUnpickler.unpickleEntry(reader.readField("$keys")).asInstanceOf[List[String]]
      val results = for (key <- keys) yield {
        val value = valueUnpickler.unpickleEntry(reader.readField(key))
        key -> value.asInstanceOf[A]
      }
      reader.endEntry()
      reader.popHints()
      results.toMap
    }
    override def toString = "StringMapPicklerUnpickler"
  }

  implicit def vectorPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T], collTag: FastTypeTag[Vector[T]], cbf: CanBuildFrom[Vector[T], T, Vector[T]]): SPickler[Vector[T]] with Unpickler[Vector[T]] =
    mkTravPickler[T, Vector[T]]
  implicit def arrayPickler[A >: Null: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Array[A]], cbf: CanBuildFrom[Array[A], A, Array[A]]): SPickler[Array[A]] with Unpickler[Array[A]] =
    mkTravPickler[A, Array[A]]

  implicit def listPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    collTag: FastTypeTag[List[A]]): SPickler[List[A]] with Unpickler[List[A]] =
    mkTravPickler[A, List[A]]

  // Ideally we wouldn't have this one, but it some sbt tasks return Seq
  implicit def seqPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Seq[A]], cbf: CanBuildFrom[Seq[A], A, Seq[A]]): SPickler[Seq[A]] with Unpickler[Seq[A]] =
    mkTravPickler[A, Seq[A]]

  implicit class RichType(tpe: scala.reflect.api.Universe#Type) {
    import definitions._
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }
  }

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
        if (isPrimitive) builder.unpinHints()
        builder.popHints()
        builder.endCollection()
        builder.endEntry()
      }

      def unpickle(tpe: String, preader: PReader): Any = {
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
          val elem = elemUnpickler.unpickleEntry(reader.readElement())
          builder += elem.asInstanceOf[A]
          i = i + 1
        }
        reader.unpinHints()
        preader.popHints()
        preader.endCollection()
        builder.result
      }
    }
}
