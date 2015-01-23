package sbt

package object serialization {
  type SPickler[A] = scala.pickling.SPickler[A]
  val SPickler = scala.pickling.SPickler
  type Unpickler[A] = scala.pickling.Unpickler[A]
  val Unpickler = scala.pickling.Unpickler
  val functions: SerializationFunctions = new SerializationFunctions {}

  // pickling macros need FastTypeTag$ to have been initialized;
  // if things ever compile with this removed, it can be removed.
  private val __forceInitializeFastTypeTagCompanion = scala.pickling.FastTypeTag
}
