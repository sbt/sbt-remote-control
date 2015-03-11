package sbt
package server

object Serializations {
  val key = AttributeKey[DynamicSerialization]("Aggregate of registered serializations")

  def extractOpt(state: State): Option[DynamicSerialization] = state get key
  def update(state: State): State = {
    val extracted = Project.extract(state)
    val serializations = extracted.get(SerializersKeys.registeredSerializers).foldLeft(DynamicSerialization.defaultSerializations) { (sofar, next) =>
      sofar.register(next.serializer)(next.manifest)
    }
    state.put(key, serializations)
  }
}

object Conversions {
  val key = AttributeKey[DynamicConversion]("Aggregate of registered conversions")

  def extractOpt(state: State): Option[DynamicConversion] = state get key
  def update(state: State): State = {
    val extracted = Project.extract(state)
    val conversions = extracted.get(SerializersKeys.registeredProtocolConversions)
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
      register(compileFailedExceptionToProtocol),
      register(moduleIdToProtocol),
      register(seqAttributedFileToProtocol))
    conversions.foldLeft(base) { (sofar, next) =>
      next(sofar)
    }
  }
}
