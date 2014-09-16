package sbt.client

import java.net.URI

/** This is a wrapper around scoped keys which you can use to promote typesafe interfaces. */
final case class TaskKey[T](key: sbt.protocol.ScopedKey) {
  // TODO - scope changing methods.

  def in(build: URI): TaskKey[T] = TaskKey[T](key.copy(scope = key.scope.copy(build = Some(build))))
  def in(project: sbt.protocol.ProjectReference): TaskKey[T] =

    TaskKey[T](key.copy(scope =
      key.scope.copy(
        project = Some(project),
        build = Some(project.build))))
  // TODO - make this typesafe!!!!
  def in(config: String): TaskKey[T] = TaskKey[T](key.copy(scope = key.scope.copy(config = Some(config))))

  // TODO - in with a key as the option...
}