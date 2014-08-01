package sbt
package protocol

import play.api.libs.json.{ Format, Reads, Writes }

/**
 * An interface representing a mechanism to register and
 *  retrieve serialization for specific types at runtime.
 *
 *  This suffers from all the same limitations as scala.Manifest for
 *  handling types.
 */
trait DynamicSerialization {
  def register[T](serializer: Format[T])(implicit mf: Manifest[T]): Unit
  def lookup[T](implicit mf: Manifest[T]): Option[Format[T]]
}
/**
 * A registry for serialization mechanisms.  Sbt plugins/builds can
 * use this to define a mechanism to serialize/deserialize their
 * values between the sbt-server and consumers.
 *
 * NOTE: Users should NOT use this directly
 */
@deprecated("Users should not use this object directly")
object DynamicSerialization extends DynamicSerialization {
  // Here we store erased types
  private type RawManifest = Manifest[_]
  private type RawFormat = Format[_]
  private val registered =
    scala.collection.concurrent.TrieMap[RawManifest, RawFormat]()

  override def register[T](serializer: Format[T])(implicit mf: Manifest[T]): Unit =
    // Here we erase the original type when storing
    registered.put(mf, serializer)

  override def lookup[T](implicit mf: Manifest[T]): Option[Format[T]] =
    // When looking up, given the interface, it's safe to return to
    // the original type.
    (registered get mf).asInstanceOf[Option[Format[T]]] orElse
      // TODO - When generating a default serializer, we should probably also memoize it.
      memoizedDefaultSerializer(mf)

  // TODO - This should be a registration system and not so hacky...
  private def memoizedDefaultSerializer[T](mf: Manifest[T]): Option[Format[T]] =
    defaultSerializer(mf) match {
      case Some(s) =>
        register(s)(mf)
        Some(s)
      case None => None
    }
  /**
   * This represents the generic way in which we can serialize sbt settings over the network.
   *
   * This is the ONLY list we use when attempting to inspect unknown types.  If we don't
   * have a mechanism here, we can't serialize (on either side) and we wind up with a
   * None representing the semantic value, but the "toString" will still make it across.
   */
  private def defaultSerializer[T](mf: Manifest[T]): Option[Format[T]] = {
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
          child <- lookup(mf.typeArguments(0))
        } yield {
          optionFormat(child.asInstanceOf[Format[Any]])
        }
      // TODO - polymorphism?
      case Classes.SeqSubClass() =>
        // Now we need to find the first type arguments structure:
        import collection.generic.CanBuildFrom
        for {
          child <- lookup(mf.typeArguments(0))
        } yield {
          val reads = Reads.traversableReads[Seq, Any](collection.breakOut, child.asInstanceOf[Reads[Any]])
          val writes = Writes.traversableWrites(child.asInstanceOf[Writes[Any]])
          Format(reads, writes)
        }
      case Classes.AttributedSubClass() =>
        for {
          child <- lookup(mf.typeArguments(0))
        } yield attributedFormat(child)
      case _ =>
        System.err.println("DEBUGME - Error:  No way to serialize: " + mf)
        None
    }).asInstanceOf[Option[Format[T]]]
  }

}