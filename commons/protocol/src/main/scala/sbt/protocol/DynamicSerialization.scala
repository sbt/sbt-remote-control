package sbt
package protocol

import play.api.libs.json.{ Format, Reads, Writes }

trait ReadOnlyDynamicSerialization {
  def lookup[T](implicit mf: Manifest[T]): Option[Format[T]]
}

/**
 * An interface representing a mechanism to register and
 *  retrieve serialization for specific types at runtime.
 *
 *  This suffers from all the same limitations as scala.Manifest for
 *  handling types.
 */
trait DynamicSerialization extends ReadOnlyDynamicSerialization {
  def register[T](serializer: Format[T])(implicit mf: Manifest[T]): Unit
}

// TODO this should be private[sbt] somehow but right now we are using it in com.typesafe
// instead of sbt package
case class ImmutableDynamicSerialization(registered: Map[Manifest[_], Format[_]]) extends ReadOnlyDynamicSerialization {
  def register[T](serializer: Format[T])(implicit mf: Manifest[T]): ImmutableDynamicSerialization =
    // Here we erase the original type when storing
    ImmutableDynamicSerialization(registered + (mf -> serializer))

  override def lookup[T](implicit mf: Manifest[T]): Option[Format[T]] =
    // When looking up, given the interface, it's safe to return to
    // the original type.
    (registered get mf).asInstanceOf[Option[Format[T]]] orElse
      DynamicSerialization.memoizedDefaultSerializer(mf)
}

object ImmutableDynamicSerialization {
  val defaultSerializations = ImmutableDynamicSerialization(Map.empty)
}

// TODO this should be private[sbt] somehow but right now we are using it in com.typesafe
// instead of sbt package
class MutableDynamicSerialization extends DynamicSerialization {
  @volatile
  private var underlying = ImmutableDynamicSerialization.defaultSerializations

  override def register[T](serializer: Format[T])(implicit mf: Manifest[T]): Unit = synchronized {
    underlying = underlying.register(serializer)
  }

  override def lookup[T](implicit mf: Manifest[T]): Option[Format[T]] =
    underlying.lookup(mf)

  def immutableSnapshot: ReadOnlyDynamicSerialization = underlying
}

private object DynamicSerialization {
  private val defaultSerializationMemos =
    scala.collection.concurrent.TrieMap[Manifest[_], Format[_]]()

  def memoizedDefaultSerializer[T](mf: Manifest[T]): Option[Format[T]] =
    defaultSerializer(mf) match {
      case Some(s) =>
        defaultSerializationMemos.put(mf, s)
        Some(s)
      case None => None
    }

  private def defaultSerializer[T](mf: Manifest[T]): Option[Format[T]] = defaultSerializationMemos.get(mf).map(_.asInstanceOf[Format[T]]) orElse {
    (mf.erasure match {
      case Classes.StringClass => Some(implicitly[Format[String]])
      case Classes.FileClass => Some(implicitly[Format[java.io.File]])
      case Classes.BooleanClass => Some(implicitly[Format[Boolean]])
      case Classes.ShortClass => Some(implicitly[Format[Short]])
      case Classes.IntClass => Some(implicitly[Format[Int]])
      case Classes.LongClass => Some(implicitly[Format[Long]])
      case Classes.FloatClass => Some(implicitly[Format[Float]])
      case Classes.DoubleClass => Some(implicitly[Format[Double]])
      case Classes.OptionSubClass() =>
        for {
          child <- memoizedDefaultSerializer(mf.typeArguments(0))
        } yield {
          optionFormat(child.asInstanceOf[Format[Any]])
        }
      // TODO - polymorphism?
      case Classes.SeqSubClass() =>
        // Now we need to find the first type arguments structure:
        import collection.generic.CanBuildFrom
        for {
          child <- memoizedDefaultSerializer(mf.typeArguments(0))
        } yield {
          val reads = Reads.traversableReads[Seq, Any](collection.breakOut, child.asInstanceOf[Reads[Any]])
          val writes = Writes.traversableWrites(child.asInstanceOf[Writes[Any]])
          Format(reads, writes)
        }
      case Classes.AttributedSubClass() =>
        for {
          child <- memoizedDefaultSerializer(mf.typeArguments(0))
        } yield attributedFormat(child)
      case _ =>
        System.err.println("DEBUGME - Error:  No way to serialize: " + mf)
        None
    }).asInstanceOf[Option[Format[T]]]
  }

}
