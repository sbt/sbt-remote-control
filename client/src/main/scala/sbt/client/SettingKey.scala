package sbt
package client

import java.net.URI

/** This is a wrapper around scoped keys which you can use to promote typesafe interfaces. */
final case class SettingKey[T](key: protocol.ScopedKey) {
  // TODO - scope changing methods.

  def in(build: URI): SettingKey[T] = SettingKey[T](key.copy(scope = key.scope.copy(build = Some(build))))
  def in(project: protocol.ProjectReference): SettingKey[T] =
    SettingKey[T](key.copy(scope =
      key.scope.copy(
        project = Some(project),
        build = Some(project.build))))
  // TODO - make this typesafe!!!!
  def in(config: String): SettingKey[T] = SettingKey[T](key.copy(scope = key.scope.copy(config = Some(config))))

  // TODO - in with a key as the option...
}