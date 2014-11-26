package sbt

import scala.util.control.NonFatal

package object serialization extends sbt.serialization.SerializationPicklerUnpickler {
  import scala.pickling._

  // pickling macros need FastTypeTag$ to have been initialized;
  // if things ever compile with this removed, it can be removed.
  private val __forceInitializeFastTypeTagCompanion = FastTypeTag

  // this "newtypes" SPickler so we can require importing sbt.serialization._
  // and avoid the need to import scala.pickling.SPickler
  // FIXME maybe this should extend AnyVal but it was causing
  // "bridge generated for member method clashes with definition
  // of the member itself" errors
  /**
   * A pickler for sbt; wraps a scala.pickling.SPickler. When defining picklers,
   * define a scala.pickling.SPickler and let this be created implicitly.
   * When requiring a pickler for sbt purposes, require this type instead of
   * scala.pickling.SPickler directly.
   */
  final class SbtPickler[T] private[serialization] (val underlying: SPickler[T], val tag: FastTypeTag[T])

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtPicklerFromSPickler[T](implicit spickler: SPickler[T], tag: FastTypeTag[T]): SbtPickler[T] =
    new SbtPickler[T](spickler, tag)

  // FIXME maybe this should extend AnyVal but it was causing
  // "bridge generated for member method clashes with definition
  // of the member itself" errors
  /**
   * An unpickler for sbt; wraps a scala.pickling.Unpickler. When defining unpicklers,
   * define a scala.pickling.Unpickler and let this be created implicitly.
   * When requiring an unpickler for sbt purposes, require this type instead of
   * scala.pickling.Unpickler directly.
   */
  final class SbtUnpickler[T] private[serialization] (val underlying: Unpickler[T])

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtUnpicklerFromUnpickler[T](implicit unpickler: Unpickler[T]): SbtUnpickler[T] =
    new SbtUnpickler[T](unpickler)

  /**
   * A pair of pickler and unpickler; don't define this by hand, just let
   * it be implicitly created when you define a scala.pickling.SPickler and
   * scala.pickling.Unpickler (or use the default definitions for those).
   */
  final class SbtSerializer[T] private[serialization] (val pickler: SbtPickler[T], val unpickler: SbtUnpickler[T])

  object SbtSerializer {
    def apply[T](pickler: SbtPickler[T], unpickler: SbtUnpickler[T]): SbtSerializer[T] =
      new SbtSerializer[T](pickler, unpickler)
  }

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtSerializerFromPicklerAndUnpickler[T](implicit pickler: SbtPickler[T], unpickler: SbtUnpickler[T]): SbtSerializer[T] =
    SbtSerializer[T](pickler, unpickler)
}
