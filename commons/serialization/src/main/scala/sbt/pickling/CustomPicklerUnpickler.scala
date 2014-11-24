package sbt.pickling

import java.io.File
import java.net.URI
import scala.pickling._
import scala.reflect.runtime.universe._
import scala.collection.immutable.::
import scala.collection.generic.CanBuildFrom
import org.json4s.{ JValue, JString }

object FakeTags {
  val JValue = implicitly[FastTypeTag[JValue]]
}

trait CustomPicklerUnpickler extends LowPriorityCustomPicklerUnpickler {
  private def jvaluePickler(implicit pf: PickleFormat): SPickler[JValue] with Unpickler[JValue] = new SPickler[JValue] with Unpickler[JValue] {
    val format: PickleFormat = pf
    val stringPickler = implicitly[SPickler[String]]
    val stringUnpickler = implicitly[Unpickler[String]]
    def pickle(jv: JValue, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FakeTags.JValue)
      builder.beginEntry(jv)
      builder.endEntry()
      builder.popHints()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      preader.pushHints()
      preader.hintTag(tag)
      preader.beginEntryNoTag()
      val result = preader.readPrimitive
      preader.endEntry
      preader.popHints()
      result
    }
  }
  /*
  implicit def jsonValuePickler(implicit pf: PickleFormat): SPickler[JsonValue] with Unpickler[JsonValue] = new SPickler[JsonValue] with Unpickler[JsonValue] {
    val format: PickleFormat = pf
    val jvPickler = jvaluePickler
    val jvUnpickler = jvaluePickler
    def pickle(jv: JsonValue, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FakeTags.JValue)
      jvPickler.pickle(jv.json, builder)
      builder.popHints()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      val json = jvUnpickler.unpickle(FakeTags.JValue, preader).asInstanceOf[JValue]
      JsonValue(json)
    }
  }

  implicit def serializedValuePickler(implicit pf: PickleFormat): SPickler[SerializedValue] with Unpickler[SerializedValue] = new SPickler[SerializedValue] with Unpickler[SerializedValue] {
    val format: PickleFormat = pf
    val jsonPickler = implicitly[SPickler[JsonValue]]
    val jsonUnpickler = implicitly[Unpickler[JsonValue]]
    def pickle(a: SerializedValue, builder: PBuilder): Unit =
      a match {
        case spsv: SbtPrivateSerializedValue => jsonPickler.pickle(spsv.toJson, builder)
      }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      jsonUnpickler.unpickle(tag, preader)
    }
  }

  implicit def throwablePickler(implicit pf: PickleFormat): SPickler[Throwable] with Unpickler[Throwable] = new SPickler[Throwable] with Unpickler[Throwable] {
    val format = pf
    val stringOptTag = implicitly[FastTypeTag[Option[String]]]
    val stringOptPickler = implicitly[SPickler[Option[String]]]
    val stringOptUnpickler = implicitly[Unpickler[Option[String]]]
    val throwableTag = implicitly[FastTypeTag[Throwable]]
    val throwableOptTag = implicitly[FastTypeTag[Option[Throwable]]]
    val throwableOptPicklerUnpickler = optionPickler[Throwable](throwableTag, this, this, throwableOptTag, pf)
    val vstedTag = implicitly[FastTypeTag[Vector[StackTraceElementDeserialized]]]
    val vstedPickler = implicitly[SPickler[Vector[StackTraceElementDeserialized]]]
    val vstedUnpickler = implicitly[Unpickler[Vector[StackTraceElementDeserialized]]]

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
      val cause = throwableOptPicklerUnpickler.unpickle(throwableOptTag, reader2).asInstanceOf[Option[ThrowableDeserialized]]
      val reader3 = preader.readField("stackTrace")
      reader3.hintTag(vstedTag)
      val stackTrace = vstedUnpickler.unpickle(vstedTag, reader3).asInstanceOf[Vector[StackTraceElementDeserialized]]
      val result = ThrowableDeserialized(message, cause, stackTrace)
      result.setStackTrace((stackTrace map { x =>
        new StackTraceElement(x.declaringClass, x.methodName, x.fileName, x.lineNumber)
      }).toArray)
      result
    }
  }
  */
}

trait LowPriorityCustomPicklerUnpickler {
  implicit def canToStringPickler[A: FastTypeTag](implicit canToString: CanToString[A], pf: PickleFormat): SPickler[A] with Unpickler[A] = new SPickler[A] with Unpickler[A] {
    val format: PickleFormat = pf
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
  implicit def filePickler(implicit pf: PickleFormat): SPickler[File] with Unpickler[File] =
    canToStringPickler[File](implicitly[FastTypeTag[File]], implicitly[CanToString[File]], pf)
  implicit def vectorPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T], collTag: FastTypeTag[Vector[T]], format: PickleFormat, cbf: CanBuildFrom[Vector[T], T, Vector[T]]): SPickler[Vector[T]] with Unpickler[Vector[T]] =
    mkSeqSetPickler[T, Vector]
  implicit def arrayPickler[A >: Null: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Array[A]], format: PickleFormat, cbf: CanBuildFrom[Array[A], A, Array[A]]): SPickler[Array[A]] with Unpickler[Array[A]] =
    mkTravPickler[A, Array[A]]
  implicit def nilPickler(implicit pf: PickleFormat): SPickler[Nil.type] with Unpickler[Nil.type] = new SPickler[Nil.type] with Unpickler[Nil.type] {
    val format: PickleFormat = pf
    val ccUnpickler: Unpickler[::[String]] = implicitly[Unpickler[::[String]]]
    def pickle(coll: Nil.type, builder: PBuilder): Unit = {
      builder.beginEntry(coll)
      builder.beginCollection(0)
      builder.endCollection
      builder.endEntry()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any =
      ccUnpickler.unpickle(tag, preader).asInstanceOf[Nil.type]
  }
  implicit def listUnpickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    ccPickler: SPickler[::[A]], ccUnpickler: Unpickler[::[A]],
    collTag: FastTypeTag[List[A]],
    pf: PickleFormat): SPickler[List[A]] with Unpickler[List[A]] = new SPickler[List[A]] with Unpickler[List[A]] {
    val format: PickleFormat = pf
    val np = nilPickler
    def pickle(coll: List[A], builder: PBuilder): Unit =
      coll match {
        case Nil => np.pickle(Nil, builder)
        case xs: ::[A] => ccPickler.pickle(xs, builder)
      }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any =
      ccUnpickler.unpickle(tag, preader).asInstanceOf[List[A]]
  }
  // Guard pickler
  implicit def seqPickler[A: FastTypeTag]: SPickler[Seq[A]] with Unpickler[Seq[A]] =
    sys.error("use Vector[A] or List[A] instead")
  // Guard pickler
  implicit def setPickler[A: FastTypeTag]: SPickler[Set[A]] with Unpickler[Set[A]] =
    sys.error("use Vector[A] or List[A] instead")
  // Guard pickler
  implicit def somePickler[A: FastTypeTag]: SPickler[Some[A]] with Unpickler[Some[A]] =
    sys.error("use the pickler for Option[A]")
  // Guard pickler
  implicit lazy val nonePickler: SPickler[None.type] with Unpickler[None.type] =
    sys.error("use the pickler for Option[A]")
  implicit def optionPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Option[A]],
    pf: PickleFormat): SPickler[Option[A]] with Unpickler[Option[A]] = new SPickler[Option[A]] with Unpickler[Option[A]] {
    val format: PickleFormat = pf
    val elemTag = implicitly[FastTypeTag[A]]
    val isPrimitive = elemTag.tpe.isEffectivelyPrimitive
    val nullPickler = implicitly[SPickler[Null]]

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

  implicit class RichType(tpe: scala.reflect.api.Universe#Type) {
    import definitions._
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }
  }

  def mkSeqSetPickler[A: FastTypeTag, Coll[_] <: Traversable[_]](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    pf: PickleFormat, cbf: CanBuildFrom[Coll[A], A, Coll[A]],
    collTag: FastTypeTag[Coll[A]]): SPickler[Coll[A]] with Unpickler[Coll[A]] =
    mkTravPickler[A, Coll[A]]
  def mkTravPickler[A: FastTypeTag, C <% Traversable[_]: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    pf: PickleFormat, cbf: CanBuildFrom[C, A, C],
    collTag: FastTypeTag[C]): SPickler[C] with Unpickler[C] =
    new SPickler[C] with Unpickler[C] {
      val format: PickleFormat = pf
      val elemTag = implicitly[FastTypeTag[A]]
      val isPrimitive = elemTag.tpe.isEffectivelyPrimitive

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
