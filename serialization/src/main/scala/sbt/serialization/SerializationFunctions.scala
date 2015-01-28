package sbt.serialization

import scala.pickling.Generated

/** A layer of serialization cake which provides the gen* macros for auto-constructing new picklers. */
trait SerializationFunctions {
  import scala.language.experimental.macros

  // non-implicit aliases of pickling's gen macros
  def genPickler[T]: SPickler[T] = macro scala.pickling.Compat.PicklerMacros_impl[T]
  def genUnpickler[T]: Unpickler[T] with scala.pickling.Generated = macro scala.pickling.Compat.UnpicklerMacros_impl[T]
}
