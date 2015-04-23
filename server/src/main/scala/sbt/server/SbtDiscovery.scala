package sbt
package server

import SbtToProtocolUtils._

// This is a helper class that lets us run discovery methods on sbt.
private[server] object SbtDiscovery {

  private def getRootObjectName(o: AnyRef): String = {
    val rawClassname = o.getClass.getName
    // Technically, the $ shoudl ALWAYS be there for autoplugins.
    if (rawClassname endsWith "$") rawClassname.dropRight(1)
    else rawClassname
  }

  private def projectDependencies(r: ResolvedProject): protocol.ProjectDependencies = {
    val classpath =
      for {
        dep <- r.dependencies
        to = projectRefToProtocol(dep.project)
      } yield protocol.ClasspathDep(to, dep.configuration)
    val aggregate =
      for {
        dep <- r.aggregate
      } yield projectRefToProtocol(dep)
    protocol.ProjectDependencies(classpath, aggregate)
  }

  def buildStructure(state: State): protocol.MinimalBuildStructure = {
    val extracted = sbt.Project.extract(state)
    val projects =
      (for {
        (build, unit) <- extracted.structure.units
        resolved <- unit.defined.values
        ref = projectRefToProtocol(ProjectRef(build, resolved.id))
        plugins = resolved.autoPlugins.map(getRootObjectName).toVector
      } yield protocol.MinimalProjectStructure(ref, plugins, Some(projectDependencies(resolved)))).toVector

    val buildsData =
      (for {
        (build, unit) <- extracted.structure.units
        classpath = unit.classpath.toVector
        imports = unit.imports.toVector
      } yield protocol.BuildData(build, classpath, imports)).toVector

    val builds = projects.map(_.id.build).distinct
    protocol.MinimalBuildStructure(
      builds = builds,
      buildsData = buildsData,
      projects = projects)
  }

  // have to leave the type inferencer here.
  def structure(state: State) =
    Project.extract(state).structure

  def keyIndex(state: State): sbt.KeyIndex =
    structure(state).index.keyIndex

  def builds(state: State): Set[String] =
    keyIndex(state).buildURIs map (_.toASCIIString)

  def projects(state: State, build: URI): Set[protocol.ProjectReference] =
    keyIndex(state).projects(build) map { name =>
      protocol.ProjectReference(build, name)
    }

  val TaskClass = classOf[sbt.Task[_]]
  // NOTE - This in an approximation...
  def isTaskKey[T](key: sbt.ScopedKey[T]): Boolean = {
    val mf = key.key.manifest
    mf.runtimeClass == TaskClass
  }
  def isInputKey[T](key: sbt.ScopedKey[T]): Boolean = {
    val mf = key.key.manifest
    mf.runtimeClass == classOf[sbt.InputTask[_]]
  }
}
