package sbt.protocol

import java.net.URI
import java.io.File
import sbt.serialization._

/**
 * This represents a "key" in sbt.
 * Keys have names and "types" associated.
 */
final case class AttributeKey(name: String, manifest: TypeExpression) {
  override def toString = "AttributeKey[" + manifest + "](\"" + name + "\")"
}
object AttributeKey {
  require(implicitly[Unpickler[TypeExpression]] ne null)
  implicit val unpickler: Unpickler[AttributeKey] = genUnpickler[AttributeKey]
  implicit val pickler: Pickler[AttributeKey] = genPickler[AttributeKey]

  def apply[T](name: String)(implicit mf: Manifest[T]): AttributeKey = {
    // FIXME I don't think this is really the right name we pass to TypeExpression
    AttributeKey(name, TypeExpression.parse(mf.runtimeClass.getName)._1)
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
  implicit val pickler: Pickler[ProjectReference] = genPickler[ProjectReference]
}

/**
 * Represents the scope a particular key can have in sbt.
 *
 * @param build
 *        A key is either associated with a Build, or in Global scope.
 * @param project
 *        A key is either associated with a project, or in the Build/Global scope.
 * @param config
 *        A key may be associated with a configuration axis.
 * @param task
 *        A key may optionally be associated with a task axis
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
  implicit val pickler: Pickler[SbtScope] = genPickler[SbtScope]
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
  implicit val pickler: Pickler[ScopedKey] = genPickler[ScopedKey]
}
// TODO nothing uses this?
/** A means of JSON-serializing key lists from sbt to our client. */
final case class KeyList(keys: Vector[ScopedKey])
object KeyList {
  implicit val unpickler: Unpickler[KeyList] = genUnpickler[KeyList]
  implicit val pickler: Pickler[KeyList] = genPickler[KeyList]
}

// TODO - This is a bad name...
/** The notion of classpath dependencies between projects. */
final case class ClasspathDep(
  project: ProjectReference,
  configuration: Option[String])
object ClasspathDep {
  implicit val picklerUnpickler = PicklerUnpickler.generate[ClasspathDep]
}
/** The notiion of build dependencies between projects. */
final case class ProjectDependencies(
  classpath: Seq[ClasspathDep],
  aggregate: Seq[ProjectReference])
object ProjectDependencies {
  implicit val picklerUnpickler = PicklerUnpickler.generate[ProjectDependencies]
}

/** Core information returned about projects for build clients. */
final case class MinimalProjectStructure(
  id: ProjectReference,
  // Class names of plugins used by this project.
  plugins: Vector[String],
  /** A possible list of dependencies between projects..   Added in 0.2 of the protocol. */
  dependencies: Option[ProjectDependencies])
object MinimalProjectStructure {
  implicit val unpickler: Unpickler[MinimalProjectStructure] = genUnpickler[MinimalProjectStructure]
  implicit val pickler: Pickler[MinimalProjectStructure] = genPickler[MinimalProjectStructure]
}

final case class BuildData(
  uri: URI,
  classpath: Vector[File],
  imports: Vector[String])
object BuildData {
  implicit val picklerUnpickler: Pickler[BuildData] with Unpickler[BuildData] = PicklerUnpickler.generate[BuildData]
}

final case class MinimalBuildStructure(
  builds: Vector[URI],
  buildsData: Vector[BuildData],
  projects: Vector[MinimalProjectStructure] // TODO - For each project, we may want to include a list of serializable key by configuration (minimize amount of data sent) that we can
  // "unwind" on the client side into ScopedKeys.
  )
object MinimalBuildStructure {
  implicit val unpickler: Unpickler[MinimalBuildStructure] = genUnpickler[MinimalBuildStructure]
  implicit val pickler: Pickler[MinimalBuildStructure] = genPickler[MinimalBuildStructure]
}

final case class Attributed[T](data: T)
object Attributed {
  implicit def picklerUnpickler[T](implicit dataPickler: Pickler[T],
    dataUnpickler: Unpickler[T],
    attrTag: FastTypeTag[Attributed[T]]): Pickler[Attributed[T]] with Unpickler[Attributed[T]] =
    new Pickler[Attributed[T]] with Unpickler[Attributed[T]] {

      override val tag = attrTag

      override def pickle(attr: Attributed[T], builder: PBuilder): Unit = {
        builder.hintTag(tag)

        builder.beginEntry(attr)
        builder.putField("data", { b ⇒
          b.hintTag(dataPickler.tag)
          dataPickler.pickle(attr.data, b)
        })
        builder.endEntry()
      }

      override def unpickle(tag: String, reader: PReader): Any = {
        val data = dataUnpickler.unpickleEntry(reader.readField("data")).asInstanceOf[T]
        Attributed(data)
      }
    }
}
