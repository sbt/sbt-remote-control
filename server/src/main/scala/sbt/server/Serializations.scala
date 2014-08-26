package sbt
package server

import protocol.ImmutableDynamicSerialization

object Serializations {
  val key = AttributeKey[ImmutableDynamicSerialization]("Aggregate of registered serializations")

  def extractOpt(state: State): Option[ImmutableDynamicSerialization] = state get key
  def update(state: State): State = {
    val extracted = Project.extract(state)
    val serializations = extracted.get(UIContext.registeredFormats).foldLeft(ImmutableDynamicSerialization(Map.empty)) { (sofar, next) =>
      sofar.register(next.format)(next.manifest)
    }
    state.put(key, serializations)
  }
}
