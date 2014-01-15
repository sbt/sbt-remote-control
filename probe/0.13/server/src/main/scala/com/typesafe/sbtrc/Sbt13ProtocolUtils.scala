
package com.typesafe.sbtrc

/** Helpers to map from sbt types into serializable json types. */
object Sbt13ToProtocolUtils {

  def protocolToScopedKey[T](key: protocol.ScopedKey, state: sbt.State): Option[sbt.ScopedKey[T]] = {
    // We should just actually translate things into sbt-isms.
    val extracted = sbt.Project.extract(state)
    val attributeKey = extracted.structure.index.keyMap.get(key.key.name)
    // Now convert the scope.
    import sbt._
    // TODO - This lookup sucks....
    val matchingScopes =
      for {
        setting <- sbt.Project.extract(state).structure.settings
        if SbtToProtocolUtils.scopeToProtocol(setting.key.scope) == key.scope
      } yield setting.key.scope
    for {
      key <- attributeKey
      s <- matchingScopes.headOption
    } yield Def.ScopedKey(s, key.asInstanceOf[sbt.AttributeKey[T]])
  }

}