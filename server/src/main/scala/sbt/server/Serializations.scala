package sbt
package server

import protocol.DynamicSerialization

object Serializations {
  val key = AttributeKey[DynamicSerialization]("Aggregate of registered serializations")

  def extractOpt(state: State): Option[DynamicSerialization] = state get key
  def update(state: State): State = {
    val extracted = Project.extract(state)
    val serializations = extracted.get(UIContext.registeredFormats).foldLeft(DynamicSerialization.defaultSerializations) { (sofar, next) =>
      sofar.register(next.format)(next.manifest)
    }
    state.put(key, serializations)
  }
}
