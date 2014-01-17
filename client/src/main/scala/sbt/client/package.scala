package sbt

package object client {
  // API from the remote protocol directly
  type KeyFilter = com.typesafe.sbtrc.protocol.KeyFilter
  type ProjectReference = com.typesafe.sbtrc.protocol.ProjectReference
  type ScopedKey = com.typesafe.sbtrc.protocol.ScopedKey
  type TaskResult[T] = com.typesafe.sbtrc.protocol.TaskResult[T]
  type MinimalBuildStructure = com.typesafe.sbtrc.protocol.MinimalBuildStructure

  // Events from protocol API.
  type Event = com.typesafe.sbtrc.protocol.Event

  // Wrapper names for functions
  type BuildStructureListener = MinimalBuildStructure => Unit
  type ValueListener[T] = (ScopedKey, TaskResult[T]) => Unit
  type EventListener = Event => Unit
}