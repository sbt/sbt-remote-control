package sbt

import scala.util.Try

package object client {
  // Wrapper names for functions
  type BuildStructureListener = protocol.MinimalBuildStructure => Unit
  type RawValueListener = (protocol.ScopedKey, protocol.TaskResult) => Unit
  type ValueListener[T] = (protocol.ScopedKey, Try[T]) => Unit
  type EventListener = protocol.Event => Unit
}
