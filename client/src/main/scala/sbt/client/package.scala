package sbt

package object client {
  // Wrapper names for functions
  type BuildStructureListener = protocol.MinimalBuildStructure => Unit
  type ValueListener[T] = (protocol.ScopedKey, protocol.TaskResult[T, Throwable]) => Unit
  type EventListener = protocol.Event => Unit
}