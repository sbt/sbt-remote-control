package sbt
package protocol

import play.api.libs.json.{ Format, Reads, Writes }

/**
 * An interface representing a mechanism to register and
 *  retrieve serialization for specific types at runtime.
 *
 *  This suffers from all the same limitations as scala.Manifest for
 *  handling types.
 *
 *  A DynamicSerialization is immutable.
 */
trait DynamicSerialization {
  /** Look up a serialization using its type manifest */
  def lookup[T](implicit mf: Manifest[T]): Option[Format[T]]
  /** Add a serializer, returning the new modified DynamicSerialization. */
  def register[T](serializer: Format[T])(implicit mf: Manifest[T]): DynamicSerialization
}

object DynamicSerialization {
  val defaultSerializations: DynamicSerialization = ConcreteDynamicSerialization(Map.empty)
}

private final case class ConcreteDynamicSerialization(registered: Map[Manifest[_], Format[_]]) extends DynamicSerialization {
  override def register[T](serializer: Format[T])(implicit mf: Manifest[T]): DynamicSerialization =
    // Here we erase the original type when storing
    ConcreteDynamicSerialization(registered + (mf -> serializer))

  override def lookup[T](implicit mf: Manifest[T]): Option[Format[T]] =
    // When looking up, given the interface, it's safe to return to
    // the original type.
    (registered get mf).asInstanceOf[Option[Format[T]]] orElse
      ConcreteDynamicSerialization.memoizedDefaultSerializer(mf)
}

private object ConcreteDynamicSerialization {
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
