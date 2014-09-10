package sbt.protocol

import java.net.URI
import ScalaShims.ManifestFactory
import play.api.libs.json._

/**
 *  Represents the type information we can serialize over a network
 *  from sbt.  We try to preserve, as much as possible, the items inside
 *  a scala.reflect.Manfiest.
 */
final case class TypeInfo(erasureClass: String, typeArguments: Seq[TypeInfo] = Seq.empty) {
  override def toString = erasureClass + (
    if (typeArguments.isEmpty) ""
    else typeArguments.mkString("[", ",", "]"))

  def toManifest(cl: ClassLoader = TypeInfo.getClass.getClassLoader): Option[Manifest[_]] = {
    val args = typeArguments.map(_.toManifest(cl))
    if (args.forall(_.isDefined)) {
      val realArgs = args.map(_.get)
      // Now we look at our class....
      erasureClass match {
        // TODO - Special casing classes...
        case "boolean" => Some(ManifestFactory.Boolean)
        case "short" => Some(ManifestFactory.Short)
        case "int" => Some(ManifestFactory.Int)
        case "long" => Some(ManifestFactory.Long)
        case "float" => Some(ManifestFactory.Float)
        case "double" => Some(ManifestFactory.Double)
        case "Unit" => Some(ManifestFactory.Unit)
        case default => try {
          val ourClass = cl.loadClass(erasureClass)
          val mf =
            if (realArgs.isEmpty) {
              ManifestFactory.classType(ourClass)
            } else {
              ManifestFactory.classType(ourClass, realArgs.head, realArgs.tail: _*)
            }
          Some(mf.asInstanceOf[Manifest[_]])
        } catch {
          case _: ClassNotFoundException =>
            None
        }
      }
    } else None
  }
}
object TypeInfo {
  def fromManifest[T](mf: Manifest[T]): TypeInfo = {
    TypeInfo(
      mf.erasure.getName,
      mf.typeArguments map (x => fromManifest(x)))
  }

  def fromClass[T](klass: Class[T]): TypeInfo = {
    TypeInfo(klass.getName, Nil)
  }

  implicit val format = Json.format[TypeInfo]
}

/**
 * This represents a "key" in sbt.
 *  Keys have names and "types" associated.
 */
final case class AttributeKey(name: String, manifest: TypeInfo) {
  override def toString = "AttributeKey[" + manifest + "](\"" + name + "\")"
}
object AttributeKey {
  require(implicitly[Format[TypeInfo]] ne null)
  implicit val format = Json.format[AttributeKey]
}

/**
 * Represents a project in sbt.  All projects have an associated build
 * and a name.
 */
final case class ProjectReference(build: URI, name: String)
object ProjectReference {
  implicit val format = Json.format[ProjectReference]
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
  require(implicitly[Format[ProjectReference]] ne null)
  require(implicitly[Format[URI]] ne null)
  require(implicitly[Format[AttributeKey]] ne null)
  implicit val format: Format[SbtScope] = Json.format[SbtScope]
}

/** Represents a key attached to some scope inside sbt. */
final case class ScopedKey(key: AttributeKey, scope: SbtScope) {
  override def toString =
    key + " in " + scope
}
object ScopedKey {
  require(implicitly[Format[SbtScope]] ne null)
  require(implicitly[Format[AttributeKey]] ne null)
  implicit val format: Format[ScopedKey] = Json.format[ScopedKey]
}
/** A means of JSON-serializing key lists from sbt to our client. */
final case class KeyList(keys: Seq[ScopedKey])
object KeyList {
  implicit val format: Format[KeyList] = Json.format[KeyList]
}

/** Core information returned about projects for build clients. */
final case class MinimalProjectStructure(
  id: ProjectReference,
  // Class names of plugins used by this project.
  plugins: Seq[String])
object MinimalProjectStructure {
  implicit val format = Json.format[MinimalProjectStructure]
}

final case class MinimalBuildStructure(
  builds: Seq[URI],
  projects: Seq[MinimalProjectStructure] // TODO - For each project, we may want to incldue a list of serializable key by configuration (minimize amount of data sent) that we can
  // "unwind" on the client side into ScopedKeys.
  )
object MinimalBuildStructure {
  implicit val format = Json.format[MinimalBuildStructure]
}

/** A filter for which keys to display. */
final case class KeyFilter(project: Option[String] = None,
  config: Option[String] = None,
  key: Option[String] = None) {
  def withProject(name: String) = copy(project = Some(name))
  def withConfig(name: String) = copy(config = Some(name))
  def withKeyScope(name: String) = copy(key = Some(name))
}
object KeyFilter {
  val empty = KeyFilter(None, None, None)
  implicit val format = Json.format[KeyFilter]
}
