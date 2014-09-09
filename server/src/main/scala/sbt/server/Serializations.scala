package sbt
package server

import protocol.DynamicSerialization
import protocol.DynamicConversion

object Serializations {
  val key = AttributeKey[DynamicSerialization]("Aggregate of registered serializations")

  def extractOpt(state: State): Option[DynamicSerialization] = state get key
  def update(state: State): State = {
    val extracted = Project.extract(state)
    val serializations = extracted.get(UIKeys.registeredFormats).foldLeft(DynamicSerialization.defaultSerializations) { (sofar, next) =>
      sofar.register(next.format)(next.manifest)
    }
    state.put(key, serializations)
  }
}

object Conversions {
  val key = AttributeKey[DynamicConversion]("Aggregate of registered conversions")

  def extractOpt(state: State): Option[DynamicConversion] = state get key
  def update(state: State): State = {
    val extracted = Project.extract(state)
    val conversions = extracted.get(UIKeys.registeredProtocolConversions).foldLeft(DynamicConversion.empty) { (sofar, next) =>
      sofar.register(next.convert)(next.fromManifest, next.toManifest)
    }
    state.put(key, conversions)
  }
}
