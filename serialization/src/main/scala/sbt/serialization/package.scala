package sbt

import scala.pickling.FastTypeTag

package object serialization {
  val functions: SerializationFunctions = new SerializationFunctions {}

  // pickling macros need FastTypeTag$ to have been initialized;
  // if things ever compile with this removed, it can be removed.
  private val __forceInitializeFastTypeTagCompanion = FastTypeTag
}
