package sbt.protocol

import java.net.URI
import sbt.serialization._
import scala.pickling.{ SPickler, Unpickler }
import scala.pickling.internal.AppliedType

// TODO - needed for genPickler for now.
import scala.pickling.ops._
/**
 * This represents a "key" in sbt.
 *  Keys have names and "types" associated.
 * TODO FIXME it is bad to have an internal type from pickling here
 */
final case class AttributeKey(name: String, manifest: AppliedType) {
  override def toString = "AttributeKey[" + manifest + "](\"" + name + "\")"
}
object AttributeKey {
  require(implicitly[Unpickler[AppliedType]] ne null)
  implicit val unpickler: Unpickler[AttributeKey] = genUnpickler[AttributeKey]
  implicit val pickler: SPickler[AttributeKey] = genPickler[AttributeKey]

  def apply[T](name: String)(implicit mf: Manifest[T]): AttributeKey = {
    // FIXME I don't think this is really the right name we pass to AppliedType
    AttributeKey(name, AppliedType.parse(mf.runtimeClass.getName)._1)
  }
}

/**
 * Represents a project in sbt.  All projects have an associated build
 * and a name.
 */
final case class ProjectReference(build: URI, name: String)
object ProjectReference {
  require(implicitly[Unpickler[java.net.URI]] ne null)
  implicit val unpickler: Unpickler[ProjectReference] = genUnpickler[ProjectReference]
  implicit val pickler: SPickler[ProjectReference] = genPickler[ProjectReference]
}

/**
 * Represents the scope a particular key can have in sbt.
 *
 * @param build - A key is either associated witha  Build, or in Global scope.
 * @param project - A key is either associated with a project, or in the Build/Global scope.
 * @param config - A key may be associated with a configuration axis.
 * @param task - A key may optionally be associated with a task axis
 */
final case class SbtScope(build: Option[URI] = None,
  project: Option[ProjectReference] = None,
  config: Option[String] = None,
  task: Option[AttributeKey] = None) {
  override def toString = {
    val bs = build.map(b => "Build: " + b.toASCIIString + ", ").getOrElse("Global")
    val ps = project.map(b => ", Project: " + b).getOrElse("")
    val cs = config.map(b => ", Config: " + b).getOrElse("")
    val ts = task.map(b => ", Task: " + b).getOrElse("")
    "Scope(" + bs + ps + cs + ts + ")"
  }
}
object SbtScope {
  require(implicitly[Unpickler[ProjectReference]] ne null)
  require(implicitly[Unpickler[URI]] ne null)
  require(implicitly[Unpickler[AttributeKey]] ne null)
  implicit val unpickler: Unpickler[SbtScope] = genUnpickler[SbtScope]
  implicit val pickler: SPickler[SbtScope] = genPickler[SbtScope]
}

/** Represents a key attached to some scope inside sbt. */
final case class ScopedKey(key: AttributeKey, scope: SbtScope) {
  override def toString =
    key + " in " + scope
}
object ScopedKey {
  require(implicitly[Unpickler[SbtScope]] ne null)
  require(implicitly[Unpickler[AttributeKey]] ne null)
  implicit val unpickler: Unpickler[ScopedKey] = genUnpickler[ScopedKey]
  implicit val pickler: SPickler[ScopedKey] = genPickler[ScopedKey]
}
/** A means of JSON-serializing key lists from sbt to our client. */
final case class KeyList(keys: Vector[ScopedKey])
object KeyList {
  implicit val unpickler: Unpickler[KeyList] = genUnpickler[KeyList]
  implicit val pickler: SPickler[KeyList] = genPickler[KeyList]
}

/** Core information returned about projects for build clients. */
final case class MinimalProjectStructure(
  id: ProjectReference,
  // Class names of plugins used by this project.
  plugins: Vector[String])
object MinimalProjectStructure {
  implicit val unpickler: Unpickler[MinimalProjectStructure] = genUnpickler[MinimalProjectStructure]
  implicit val pickler: SPickler[MinimalProjectStructure] = genPickler[MinimalProjectStructure]
}

final case class MinimalBuildStructure(
  builds: Vector[URI],
  projects: Vector[MinimalProjectStructure] // TODO - For each project, we may want to incldue a list of serializable key by configuration (minimize amount of data sent) that we can
  // "unwind" on the client side into ScopedKeys.
  )
object MinimalBuildStructure {
  implicit val unpickler: Unpickler[MinimalBuildStructure] = genUnpickler[MinimalBuildStructure]
  implicit val pickler: SPickler[MinimalBuildStructure] = genPickler[MinimalBuildStructure]
}
