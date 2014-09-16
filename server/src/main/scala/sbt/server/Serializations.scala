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
    val conversions = extracted.get(UIKeys.registeredProtocolConversions)
      .foldLeft(addBuiltinConversions(DynamicConversion.empty)) { (sofar, next) =>
        sofar.register(next.convert)(next.fromManifest, next.toManifest)
      }
    state.put(key, conversions)
  }

  private def register[F, T](convert: F => T)(implicit fromManifest: Manifest[F], toManifest: Manifest[T]): DynamicConversion => DynamicConversion = { base =>
    base.register(convert)(fromManifest, toManifest)
  }

  private def addBuiltinConversions(base: DynamicConversion): DynamicConversion = {
    import SbtToProtocolUtils._
    val conversions = Seq(
      register(analysisToProtocol),
      register(stampToProtocol),
      register(stampsToProtocol),
      register(sourceInfoToProtocol),
      register(sourceInfosToProtocol),
      register(problemToProtocol),
      register(apisToProtocol),
      register(sourceToProtocol),
      register(relationsSourceToProtocol),
      register(relationsToProtocol),
      register(outputSettingToProtocol),
      register(compilationToProtocol),
      register(compilationsToProtocol),
      register(pathToProtocol),
      register(pathComponentToProtocol),
      register(typeParameterToProtocol),
      register(simpleTypeToProtocol),
      register(typeToProtocol),
      register(annotationArgumentToProtocol),
      register(annotationToProtocol),
      register(modifiersToProtocol),
      register(qualifierToProtocol),
      register(accessToProtocol),
      register(sourceAPIToProtocol),
      register(definitionToProtocol),
      register(packageAPIToProtocol),
      register(compileFailedExceptionToProtocol),
      register(moduleIdToProtocol))

    conversions.foldLeft(base) { (sofar, next) =>
      next(sofar)
    }
  }
}
