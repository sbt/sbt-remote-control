package sbt.serialization

/**
 * A pair of pickler and unpickler; don't define this by hand, just let
 * it be implicitly created when you define a sbt.serialization.SPickler and
 * sbt.serialization.Unpickler (or use the default definitions for those).
 */
final class SbtSerializer[T] private[serialization] (val pickler: SPickler[T], val unpickler: Unpickler[T])

object SbtSerializer {
  def apply[T](pickler: SPickler[T], unpickler: Unpickler[T]): SbtSerializer[T] =
    new SbtSerializer[T](pickler, unpickler)
}

trait SbtSerializers {
  // this is NOT in a companion object to help mandate import
  // of sbt.protocol.CoreProtocol._ to get our custom picklers
  implicit def sbtSerializerFromPicklerAndUnpickler[T](implicit pickler: SPickler[T], unpickler: Unpickler[T]): SbtSerializer[T] =
    SbtSerializer[T](pickler, unpickler)
}
