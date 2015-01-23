package sbt.serialization
package pickler

import scala.collection.generic.CanBuildFrom
import scala.pickling.{ FastTypeTag, PBuilder, PReader, PicklingException }

trait VectorPicklers {
  implicit def vectorPickler[T: FastTypeTag](implicit elemPickler: SPickler[T], elemUnpickler: Unpickler[T], collTag: FastTypeTag[Vector[T]], cbf: CanBuildFrom[Vector[T], T, Vector[T]]): SPickler[Vector[T]] with Unpickler[Vector[T]] =
    TravPickler[T, Vector[T]]
}

trait ArrayPicklers {
  implicit def arrayPickler[A >: Null: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Array[A]], cbf: CanBuildFrom[Array[A], A, Array[A]]): SPickler[Array[A]] with Unpickler[Array[A]] =
    TravPickler[A, Array[A]]
}

trait ListPicklers {
  implicit def listPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    collTag: FastTypeTag[List[A]]): SPickler[List[A]] with Unpickler[List[A]] =
    TravPickler[A, List[A]]
}

trait SeqPicklers {
  // Ideally we wouldn't have this one, but it some sbt tasks return Seq
  implicit def seqPickler[A: FastTypeTag](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A], collTag: FastTypeTag[Seq[A]], cbf: CanBuildFrom[Seq[A], A, Seq[A]]): SPickler[Seq[A]] with Unpickler[Seq[A]] =
    TravPickler[A, Seq[A]]
}

object TravPickler {
  def apply[A: FastTypeTag, C <% Traversable[_]](implicit elemPickler: SPickler[A], elemUnpickler: Unpickler[A],
    cbf: CanBuildFrom[C, A, C], collTag: FastTypeTag[C]): SPickler[C] with Unpickler[C] =
    new SPickler[C] with Unpickler[C] with RichTypes {
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
