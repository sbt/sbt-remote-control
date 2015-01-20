package sbt

import scala.util.control.NonFatal

package object serialization extends sbt.serialization.SerializationPicklerUnpickler {
  import scala.pickling.{ SPickler, Unpickler, FastTypeTag }
  import scala.pickling.ops._

  implicit val StaticOnly = scala.pickling.static.StaticOnly

  // pickling macros need FastTypeTag$ to have been initialized;
  // if things ever compile with this removed, it can be removed.
  private val __forceInitializeFastTypeTagCompanion = FastTypeTag

  /**
   * A pair of pickler and unpickler; don't define this by hand, just let
   * it be implicitly created when you define a scala.pickling.SPickler and
   * scala.pickling.Unpickler (or use the default definitions for those).
   */
  final class SbtSerializer[T] private[serialization] (val pickler: SPickler[T], val unpickler: Unpickler[T])

  object SbtSerializer {
    def apply[T](pickler: SPickler[T], unpickler: Unpickler[T]): SbtSerializer[T] =
      new SbtSerializer[T](pickler, unpickler)
  }

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtSerializerFromPicklerAndUnpickler[T](implicit pickler: SPickler[T], unpickler: Unpickler[T]): SbtSerializer[T] =
    SbtSerializer[T](pickler, unpickler)
}
