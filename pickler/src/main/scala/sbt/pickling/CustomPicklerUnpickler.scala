package sbt.pickling

import scala.pickling._
import scala.reflect.runtime.universe._
import java.io.File
import scala.collection.immutable.::
import scala.collection.generic.CanBuildFrom

trait CustomPicklerUnpickler {
  implicit def filePickler(implicit pf: PickleFormat): SPickler[File] with Unpickler[File] = new SPickler[File] with Unpickler[File] {
    val stringPickler = implicitly[SPickler[String]]
    val stringUnpickler = implicitly[Unpickler[String]]
    val format: PickleFormat = pf

    def pickle(value: File, builder: PBuilder): Unit = {
      builder.beginEntry(value)
      builder.pushHints()
      builder.putField("$value", { b =>
        b.hintTag(FastTypeTag.String)
        stringPickler.pickle(value.toString, b) 
      })
      builder.popHints()
      builder.endEntry()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      preader.pushHints()
      val result = preader.readPrimitive()
      preader.popHints()
      result
    }    
  }

  implicit def arrayPickler[A >: Null: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Array[A]], format: PickleFormat, cbf: CanBuildFrom[Array[A], A, Array[A]]): SPickler[Array[A]] with Unpickler[Array[A]] =
    mkTravPickler[A, Array[A]]
  implicit def listUnpickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
      ccPickler: SPickler[::[A]], ccUnpickler: Unpickler[::[A]],
      collTag: FastTypeTag[List[A]],
      pf: PickleFormat): SPickler[List[A]] with Unpickler[List[A]] = new SPickler[List[A]] with Unpickler[List[A]] {
    val format: PickleFormat = pf
    def pickle(coll: List[A], builder: PBuilder): Unit =
      coll match {
        case Nil =>
          builder.beginEntry(coll)
          builder.beginCollection(0)
          builder.endCollection
          builder.endEntry()
        case xs =>
          ccPickler.pickle(coll.asInstanceOf[::[A]], builder)
      }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any =
      ccUnpickler.unpickle(tag, preader).asInstanceOf[List[A]]
  }
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
          }          
          else builder.hintTag(elemTag)
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
      }
      else reader.hintTag(elemTag)
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

  def mkTravPickler[A: FastTypeTag, C <% Traversable[_]: FastTypeTag]
    (implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
              pf: PickleFormat, cbf: CanBuildFrom[C, A, C],
              collTag: FastTypeTag[C]): SPickler[C] with Unpickler[C] =
    new SPickler[C] with Unpickler[C] {
    val format: PickleFormat = pf
    val elemTag  = implicitly[FastTypeTag[A]]
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
        reader.pinHints()       // custom code here
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
