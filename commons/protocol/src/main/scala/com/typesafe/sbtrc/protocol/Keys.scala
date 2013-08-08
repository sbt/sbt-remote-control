package com.typesafe.sbtrc.protocol

import com.typesafe.sbtrc.ipc
import scala.util.parsing.json._
import com.typesafe.sbtrc.ipc.JsonReader
import java.net.URI

/** 
 *  Represents the type information we can serialize over a network
 *  from sbt.  We try to preserve, as much as possible, the items inside
 *  a scala.reflect.Manfiest.
 */
case class TypeInfo(erasureClass: String, typeArguments: Seq[TypeInfo] = Seq.empty) {
  override def toString = erasureClass+(
      if(typeArguments.isEmpty) ""
      else typeArguments.mkString("[", ",","]")
  )
}
object TypeInfo {
  implicit object MyParamertizer extends Parametizeable[TypeInfo] {
    def apply(t: TypeInfo): Map[String, Any] = 
      Map(
        "erasureClass" -> t.erasureClass,
        "typeArguments" -> t.typeArguments.map(a => Parametize(a))
      )
    def unapply(map: Map[String, Any]): Option[TypeInfo] =
      for {
        erasure <- map.get("erasureClass")
        rawArgs <- map.get("typeArguments")
        if rawArgs.isInstanceOf[Seq[_]]
        args = rawArgs.asInstanceOf[Seq[Map[String,Any]]] flatMap {
          arg => Parametize.unapply[TypeInfo](arg)
        }
      } yield TypeInfo(erasure.toString, args)
  }
}


/** This represents a "key" in sbt.
 *  Keys have names and "types" associated.
 */
case class AttributeKey(name: String, manifest: TypeInfo) {
  override def toString = "AttributeKey["+manifest+"](\""+name+"\")"
}
object AttributeKey {
  implicit object MyParamertizer extends Parametizeable[AttributeKey] {
    def apply(t: AttributeKey): Map[String, Any] = 
      Map(
        "name" -> t.name,
        "manifest" -> Parametize(t.manifest)
      )
    def unapply(map: Map[String, Any]): Option[AttributeKey] =
      for {
        name <- map.get("name")
        rawmanifest <- map.get("manifest")
        if rawmanifest.isInstanceOf[Map[_,_]]
        manifest <- Parametize.unapply[TypeInfo](rawmanifest.asInstanceOf[Map[String,Any]])
      } yield AttributeKey(name.toString, manifest)
  }
}

/**
 * Represents a project in sbt.  All projects have an associated build
 * and a name.
 */
case class ProjectReference(build: URI, name: String)
object ProjectReference {
  implicit object MyParamertizer extends Parametizeable[ProjectReference] {
    def apply(t: ProjectReference): Map[String, Any] =
      Map(
        "build" -> t.build.toASCIIString,
        "name" -> t.name
      )
    def unapply(map: Map[String, Any]): Option[ProjectReference] =
      for {
        build <- map.get("build")
        name <- map.get("name")
      } yield ProjectReference(new URI(build.toString), name.toString)
  }
}
/**
 * Represents the scope a particular key can have in sbt.
 * 
 * @param build - A key is either associated witha  Build, or in Global scope.
 * @param project - A key is either associated with a project, or in the Build/Global scope.
 * @param config - A key may be associated with a configuration axis.
 * @param task - A key may optionally be associated with a task axis
 */
case class SbtScope(build: Option[URI] = None,
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
  implicit object MyParamertizer extends Parametizeable[SbtScope] {
    def apply(t: SbtScope): Map[String, Any] = {
      val b = t.build.map(b => "build" -> b.toASCIIString).toSeq
      val p = t.project.map(p => "project" -> Parametize(p)).toSeq
      val c = t.config.map(c => "config" -> c).toSeq
      val tsk = t.task.map(t => "task" -> Parametize(t)).toSeq
      (b ++ p ++ c ++ tsk).toMap
    } 
    def unapply(map: Map[String, Any]): Option[SbtScope] = {
      val build = map.get("build").map(x => new URI(x.toString))
      val project = map.get("project").flatMap { x =>
          Parametize.unapply[ProjectReference](x.asInstanceOf[Map[String, Any]])
      }
      val config = map.get("config").map(_.toString)
      val task = map.get("task").flatMap { x =>
          Parametize.unapply[AttributeKey](x.asInstanceOf[Map[String, Any]])
      }
      Some(SbtScope(build, project, config, task))
    }
  }
}

/** Represents a key attached to some scope inside sbt. */
case class ScopedKey(key: AttributeKey, scope: SbtScope) {
  override def toString =
    key + " in " + scope
}
object ScopedKey {
  implicit object MyParamertizer extends Parametizeable[ScopedKey] {
    def apply(t: ScopedKey): Map[String, Any] =
      Map(
        "key" -> Parametize(t.key),
        "scope" -> Parametize(t.scope)
      )
    def unapply(map: Map[String, Any]): Option[ScopedKey] =
      for {
        rawKey <- map.get("key")
        if rawKey.isInstanceOf[Map[_,_]]
        key <- Parametize.unapply[AttributeKey](rawKey.asInstanceOf[Map[String,Any]])
        rawScope <- map.get("scope")
        if rawScope.isInstanceOf[Map[_,_]]
        scope<- Parametize.unapply[SbtScope](rawScope.asInstanceOf[Map[String,Any]])
      } yield ScopedKey(key, scope)
  }
}
/** A means of JSON-serializing key lists from sbt to our client. */
case class KeyList(keys: Seq[ScopedKey])
object KeyList {
  implicit object MyParamertizer extends Parametizeable[KeyList] {
    def apply(t: KeyList): Map[String, Any] =
      Map(
        "keys" -> t.keys.map(k => (Parametize(k)))
      )
    def unapply(map: Map[String, Any]): Option[KeyList] =
      for {
        rawKeys <- map.get("keys")
        if rawKeys.isInstanceOf[Seq[_]]
        keys =
          for {
            rawkey <- rawKeys.asInstanceOf[Seq[Map[String, Any]]]
            key <- Parametize.unapply[ScopedKey](rawkey)
          } yield key
      } yield KeyList(keys)
  }
}

/** A filter for which keys to display. */
case class KeyFilter(project: Option[String],
                     config: Option[String],
                     key: Option[String])
object KeyFilter {
  implicit object MyParamertizer extends Parametizeable[KeyFilter] {
    def apply(t: KeyFilter): Map[String, Any] = {
      val p = t.project.map(p => "project" -> p).toSeq
      val c = t.config.map(c => "config" -> c).toSeq
      val k = t.key.map(t => "key" -> t).toSeq
      (p ++ c ++ k).toMap
    } 
    def unapply(map: Map[String, Any]): Option[KeyFilter] = {
      val project = map.get("project").map(_.toString)
      val config = map.get("config").map(_.toString)
      val key = map.get("key").map(_.toString)
      Some(KeyFilter(project, config, key))
    }
  }
}           
                     