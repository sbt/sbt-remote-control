package sbt
package server

import sbt.protocol.TaskSuccess
import sbt.protocol.TaskResult
import sbt.protocol.BuildValue
import sbt.serialization.TypeExpression
import scala.util.control.Exception._

/** Helpers to map from sbt types into serializable json types. */
object SbtToProtocolUtils {

  def manifestToProtocol[T](mf: Manifest[T]): TypeExpression =
    TypeExpression(mf.runtimeClass.getName,
      mf.typeArguments.map(manifestToProtocol(_)))

  def keyToProtocol[T](key: sbt.AttributeKey[T]): protocol.AttributeKey =
    protocol.AttributeKey(
      key.label,
      manifestToProtocol(removeUnusedTypesFromManifest(key.manifest)))

  // Cheaty hackery to remove types that won't exist in clients from key manifests in the server API.
  private val TaskClass = classOf[sbt.Task[_]]
  private val InputTaskClass = classOf[sbt.InputTask[_]]
  private def removeUnusedTypesFromManifest[T](mf: Manifest[T]): Manifest[_] = {
    mf.runtimeClass match {
      case TaskClass | InputTaskClass => mf.typeArguments(0)
      case _ => mf
    }
  }

  def scopeToProtocol(scope: sbt.Scope): protocol.SbtScope = {
    // TODO - Is it acceptable to use configs *just by the name* or do we need
    // to actually look up the sbt instance itself.
    // if any code is using referential equality, we may need to re-evaluate this.
    val configString: Option[String] =
      scope.config.toOption.map(_.name)

    // We can only do this all at once, or not at all

    val (build, project) =
      scopedReferenceToBuildAndProject(scope.project)

    val taskString: Option[protocol.AttributeKey] =
      scope.task.toOption.map(x => keyToProtocol(x))

    // TODO - Build.... (probably from project reference)...
    protocol.SbtScope(
      build = build,
      config = configString,
      project = project,
      task = taskString)
  }

  def positionToProtocol(in: xsbti.Position): protocol.Position = {
    import xsbti.Maybe
    import java.lang.Integer
    def mi2o(mi: Maybe[Integer]): Option[Int] =
      if (mi.isDefined) Some(mi.get)
      else None
    def m2o[A](m: Maybe[A]): Option[A] =
      if (m.isDefined) Some(m.get)
      else None
    protocol.Position(sourcePath = m2o[String](in.sourcePath),
      sourceFile = m2o[java.io.File](in.sourceFile),
      line = mi2o(in.line),
      lineContent = in.lineContent,
      offset = mi2o(in.offset),
      pointer = mi2o(in.pointer),
      pointerSpace = m2o[String](in.pointerSpace))
  }

  def problemToProtocol(in: xsbti.Problem): protocol.Problem =
    protocol.Problem(category = in.category,
      severity = in.severity,
      message = in.message,
      position = positionToProtocol(in.position))

  def settingKeyToProtocolValue[T](key: SettingKey[T], state: State, extracted: Extracted): TaskResult = {
    val serializations = Serializations.extractOpt(state).getOrElse(throw new RuntimeException("state should have serializations on it"))
    val value = extracted.get(key)
    TaskSuccess(serializations.buildValue(value)(key.key.manifest))
  }

  def projectRefToProtocol(x: sbt.ProjectRef): protocol.ProjectReference = {
    protocol.ProjectReference(x.build, x.project)
  }

  def projectRefFromProtocol(x: protocol.ProjectReference): sbt.ProjectRef = {
    sbt.ProjectRef(x.build, x.name)
  }

  def scopedReferenceToBuildAndProject(axis: sbt.ScopeAxis[sbt.Reference]): (Option[java.net.URI], Option[protocol.ProjectReference]) = {
    import sbt._
    axis.toOption map {
      case x: ProjectRef =>
        val build = x.build
        Some(build) -> Some(projectRefToProtocol(x))
      case x: BuildRef =>
        Some(x.build) -> None
      case _ => throw new RuntimeException("was expected project/build reference, got: " + axis)
    } getOrElse (None -> None)
  }

  def scopedKeyToProtocol[T](key: sbt.ScopedKey[T]): protocol.ScopedKey =
    protocol.ScopedKey(
      key = keyToProtocol(key.key),
      scope = scopeToProtocol(key.scope))

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

  def compileFailedExceptionToProtocol(e: sbt.compiler.CompileFailed): protocol.CompileFailedException = {
    new protocol.CompileFailedException(e.getMessage, e.getCause, e.problems.map(problemToProtocol(_)).toVector)
  }

  def moduleIdToProtocol(moduleId: org.apache.ivy.core.module.id.ModuleId): protocol.ModuleId = {
    import scala.collection.JavaConverters._
    protocol.ModuleId(organization = moduleId.getOrganisation(), name = moduleId.getName(),
      attributes = moduleId.getAttributes().asInstanceOf[java.util.Map[String, String]].asScala.toMap)
  }

  def attributedToProtocol[T](att: sbt.Attributed[T]): protocol.Attributed[T] = {
    protocol.Attributed(att.data)
  }

  def seqAttributedFileToProtocol(attrs: Seq[sbt.Attributed[File]]): Seq[protocol.Attributed[File]] = {
    attrs.map(attributedToProtocol)
  }

  def sourcePositionToProtocol(pos: sbt.SourcePosition): sbt.protocol.SourcePosition = {
    pos match {
      case sbt.LinePosition(path, startLine) =>
        sbt.protocol.LinePosition(path, startLine)
      case sbt.NoPosition =>
        sbt.protocol.NoPosition()
      case sbt.RangePosition(path, range) =>
        sbt.protocol.RangePosition(path,
          sbt.protocol.LineRange(range.start, range.end))
    }
  }
}
