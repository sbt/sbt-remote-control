package com.typesafe.sbtrc
package api

import java.net.URI



/** This is a wrapper around scoped keys which you can use to promote typesafe interfaces. */
case class SettingKey[T](key: ScopedKey) {
  // TODO - scope changing methods.
  
  def in(build: URI): TaskKey[T] = TaskKey[T](key.copy(scope=key.scope.copy(build=Some(build))))
  def in(project: ProjectReference): TaskKey[T] = TaskKey[T](key.copy(scope=key.scope.copy(project=Some(project))))
  // TODO - make this typesafe!!!!
  def in(config: String): TaskKey[T] = TaskKey[T](key.copy(scope=key.scope.copy(config=Some(config))))

  // TODO - in with a key as the option...
}