package sbt

package object client {
  // API from the remote protocol directly
  type KeyFilter = protocol.KeyFilter
  type ProjectReference = protocol.ProjectReference
  type ScopedKey = protocol.ScopedKey
  type TaskResult[T] = protocol.TaskResult[T]
  type MinimalBuildStructure = protocol.MinimalBuildStructure

  // Events from protocol API.
  type Event = protocol.Event

  // Wrapper names for functions
  type BuildStructureListener = MinimalBuildStructure => Unit
  type ValueListener[T] = (ScopedKey, TaskResult[T]) => Unit
  type EventListener = Event => Unit
}