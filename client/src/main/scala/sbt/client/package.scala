package sbt

package object client {
  // API from the remote protocol directly
  type KeyFilter = protocol.KeyFilter
  type ProjectReference = protocol.ProjectReference
  type ScopedKey = protocol.ScopedKey
  type TaskResult[+T, +E <: Throwable] = protocol.TaskResult[T, E]
  type MinimalBuildStructure = protocol.MinimalBuildStructure
  type Completion = protocol.Completion
  type ExecutionAnalysis = protocol.ExecutionAnalysis

  // Events from protocol API.
  type Event = protocol.Event

  // Wrapper names for functions
  type BuildStructureListener = MinimalBuildStructure => Unit
  type ValueListener[T] = (ScopedKey, TaskResult[T, Throwable]) => Unit
  type EventListener = Event => Unit
}