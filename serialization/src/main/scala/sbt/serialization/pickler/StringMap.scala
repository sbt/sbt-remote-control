package sbt.serialization
package pickler

import scala.collection.generic.CanBuildFrom
import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader, PicklingException }

trait StringMapPicklers {
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
}
