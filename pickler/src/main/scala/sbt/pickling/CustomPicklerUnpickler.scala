package sbt.pickling

import scala.pickling._
import scala.reflect.runtime.universe._

trait CustomPicklerUnpickler {
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
}
