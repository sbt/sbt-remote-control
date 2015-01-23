package sbt.serialization
package pickler

import scala.pickling.{ FastTypeTag, PBuilder, PReader, PicklingException }

trait OptionPicklers extends PrimitivePicklers with RichTypes {
  implicit def optionPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Option[A]]): SPickler[Option[A]] with Unpickler[Option[A]] =
    new SPickler[Option[A]] with Unpickler[Option[A]] {
      private implicit val elemTag = implicitly[FastTypeTag[A]]
      val tag = implicitly[FastTypeTag[Option[A]]]
      private val isPrimitive = elemTag.tpe.isEffectivelyPrimitive
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
}