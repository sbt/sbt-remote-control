package sbt

/**
 * A package object which can be used to create new serializers.
 *
 * This package supports creating Pickler/Unpickler functions which can serialize arbitrary types.  See the
 * SerializedValue type for what formats this library supports serializing into.
 */
package object serialization extends SerializationFunctions with CustomPicklers with SbtSerializers {
  type SPickler[A] = scala.pickling.SPickler[A]
  val SPickler = scala.pickling.SPickler
  type Unpickler[A] = scala.pickling.Unpickler[A]
  val Unpickler = scala.pickling.Unpickler
  type SPicklerUnpickler[A] = scala.pickling.SPicklerUnpickler[A]
  val SPicklerUnpickler = scala.pickling.SPicklerUnpickler
  // These are exposed for custom implementations of picklers.
  type FastTypeTag[A] = scala.pickling.FastTypeTag[A]
  type PReader = scala.pickling.PReader
  type PBuilder = scala.pickling.PBuilder

  // pickling macros need FastTypeTag$ to have been initialized;
  // if things ever compile with this removed, it can be removed.
  private val __forceInitializeFastTypeTagCompanion = scala.pickling.FastTypeTag

  // All generated picklers are required to be static-only in this library.
  implicit val StaticOnly = scala.pickling.static.StaticOnly

  type directSubclasses = _root_.scala.pickling.directSubclasses
}
