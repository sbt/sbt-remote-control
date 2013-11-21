
package com.typesafe.sbtrc

/** Helpers to map from sbt types into serializable json types. */
object Sbt12ToProtocolUtils {

  def protocolToScopedKey[T](key: protocol.ScopedKey, state: sbt.State): sbt.ScopedKey[T] = {
    // We should just actually translate things into sbt-isms.
    val extracted = sbt.Project.extract(state)
    val attributeKey = extracted.structure.index.keyMap(key.key.name)

    // Now convert the scope.
    import sbt._
    // TODO - This lookup sucks....
    val matchingScopes =
      for {
        setting <- sbt.Project.extract(state).structure.settings
        if SbtToProtocolUtils.scopeToProtocol(setting.key.scope) == key.scope
      } yield setting.key.scope
    sbt.Project.ScopedKey(matchingScopes.head, attributeKey.asInstanceOf[sbt.AttributeKey[T]])
  }

}