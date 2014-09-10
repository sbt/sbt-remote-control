package sbt
package server

import sbt.protocol.TaskSuccess
import sbt.protocol.TaskResult
import sbt.protocol.BuildValue
import sbt.protocol.DynamicSerialization
import scala.util.control.Exception._

/** Helpers to map from sbt types into serializable json types. */
private[server] object SbtToProtocolUtils {

  def manifestToProtocol[T](mf: Manifest[T]): protocol.TypeInfo =
    protocol.TypeInfo.fromManifest(mf)

  def keyToProtocol[T](key: sbt.AttributeKey[T]): protocol.AttributeKey =
    protocol.AttributeKey(
      key.label,
      manifestToProtocol(removeUnusedTypesFromManifest(key.manifest)))

  // Cheaty hackery to remove types that won't exist in clients from key manifests in the server API.
  private val TaskClass = classOf[sbt.Task[_]]
  private val InputTaskClass = classOf[sbt.InputTask[_]]
  private def removeUnusedTypesFromManifest[T](mf: Manifest[T]): Manifest[_] = {
    mf.erasure match {
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

  def analysisToProtocol(in: sbt.inc.Analysis): protocol.Analysis =
    protocol.Analysis(stamps = stampsToProtocol(in.stamps),
      apis = apisToProtocol(in.apis),
      relations = relationsToProtocol(in.relations),
      infos = sourceInfosToProtocol(in.infos),
      compilations = compilationsToProtocol(in.compilations))

  def stampToProtocol(in: sbt.inc.Stamp): protocol.Stamp = in match {
    case x: sbt.inc.Hash => hashToProtocol(x)
    case x: sbt.inc.LastModified => lastModifiedToProtocol(x)
    case x: sbt.inc.Exists => existsToProtocol(x)
  }

  def hashToProtocol(in: sbt.inc.Hash): protocol.Hash =
    protocol.Hash(protocol.ByteArray(in.value))

  def lastModifiedToProtocol(in: sbt.inc.LastModified): protocol.LastModified =
    protocol.LastModified(in.value)

  def existsToProtocol(in: sbt.inc.Exists): protocol.Exists =
    protocol.Exists(in.value)

  def stampsToProtocol(in: sbt.inc.Stamps): protocol.Stamps =
    protocol.Stamps(allInternalSources = in.allInternalSources.toSet,
      allBinaries = in.allBinaries.toSet,
      allProducts = in.allProducts.toSet,
      sources = in.sources.mapValues(stampToProtocol),
      binaries = in.binaries.mapValues(stampToProtocol),
      products = in.products.mapValues(stampToProtocol),
      classNames = in.classNames)

  def sourceInfoToProtocol(in: sbt.inc.SourceInfo): protocol.SourceInfo =
    protocol.SourceInfo(reportedProblems = in.reportedProblems.map(problemToProtocol),
      unreportedProblems = in.unreportedProblems.map(problemToProtocol))

  def sourceInfosToProtocol(in: sbt.inc.SourceInfos): protocol.SourceInfos =
    protocol.SourceInfos(in.allInfos.mapValues(sourceInfoToProtocol))

  def problemToProtocol(in: xsbti.Problem): protocol.Problem =
    protocol.Problem(category = in.category,
      severity = in.severity,
      message = in.message,
      position = in.position)

  def apisToProtocol(in: sbt.inc.APIs): protocol.APIs =
    protocol.APIs(allExternals = in.allExternals.toSet,
      allInternalSources = in.allInternalSources.toSet,
      internal = in.internal.mapValues(sourceToProtocol),
      external = in.external.mapValues(sourceToProtocol))

  def sourceToProtocol(in: xsbti.api.Source): protocol.Source =
    protocol.Source(compilation = compilationToProtocol(in.compilation),
      hash = protocol.ByteArray(in.hash),
      api = sourceAPIToProtocol(in.api),
      apiHash = in.apiHash,
      hasMacro = in.hasMacro)

  def relationsSourceToProtocol(in: sbt.inc.Relations.Source): protocol.RelationsSource =
    protocol.RelationsSource(internal = relationToProtocol(in.internal),
      external = relationToProtocol(in.external))

  def relationToProtocol[A, B](in: sbt.Relation[A, B]): protocol.Relation[A, B] =
    protocol.Relation[A, B](forwardMap = in.forwardMap,
      reverseMap = in.reverseMap)

  def relationsToProtocol(in: sbt.inc.Relations): protocol.Relations = {
    protocol.Relations(allSources = in.allSources.toSet,
      allProducts = in.allProducts.toSet,
      allBinaryDeps = in.allBinaryDeps.toSet,
      allInternalSrcDeps = in.allInternalSrcDeps.toSet,
      allExternalDeps = in.allExternalDeps.toSet,
      srcProd = relationToProtocol(in.srcProd),
      binaryDep = relationToProtocol(in.binaryDep),
      internalSrcDep = relationToProtocol(in.internalSrcDep),
      externalDep = relationToProtocol(in.externalDep),
      direct = allCatch.opt(relationsSourceToProtocol(in.direct)),
      publicInherited = allCatch.opt(relationsSourceToProtocol(in.publicInherited)),
      classes = relationToProtocol(in.classes))

  }

  def outputSettingToProtocol(in: xsbti.api.OutputSetting): protocol.OutputSetting =
    protocol.OutputSetting(sourceDirectory = in.sourceDirectory,
      outputDirectory = in.outputDirectory)

  def compilationToProtocol(in: xsbti.api.Compilation): protocol.Compilation =
    protocol.Compilation(startTime = in.startTime,
      outputs = in.outputs.map(outputSettingToProtocol))

  def compilationsToProtocol(in: sbt.inc.Compilations): protocol.Compilations =
    protocol.Compilations(allCompilations = in.allCompilations.map(compilationToProtocol))

  def pathToProtocol(in: xsbti.api.Path): protocol.Path =
    protocol.Path(in.components.map(pathComponentToProtocol))

  def pathComponentToProtocol(in: xsbti.api.PathComponent): protocol.PathComponent = in match {
    case x: xsbti.api.Id => protocol.Id(x.id)
    case x: xsbti.api.Super => protocol.Super(pathToProtocol(x.qualifier))
    case x: xsbti.api.This => protocol.This
  }

  def typeParameterToProtocol(in: xsbti.api.TypeParameter): protocol.TypeParameter =
    protocol.TypeParameter(id = in.id,
      annotations = in.annotations.map(annotationToProtocol),
      typeParameters = in.typeParameters.map(typeParameterToProtocol),
      variance = in.variance,
      lowerBound = typeToProtocol(in.lowerBound),
      upperBound = typeToProtocol(in.upperBound))

  def simpleTypeToProtocol(in: xsbti.api.SimpleType): protocol.SimpleType = in match {
    case x: xsbti.api.Singleton => protocol.Singleton(pathToProtocol(x.path))
    case x: xsbti.api.Projection => protocol.Projection(simpleTypeToProtocol(x.prefix), x.id)
    case x: xsbti.api.Parameterized =>
      protocol.Parameterized(simpleTypeToProtocol(x.baseType), x.typeArguments.map(typeToProtocol))
    case x: xsbti.api.ParameterRef => protocol.ParameterRef(x.id)
    case x: xsbti.api.EmptyType => protocol.EmptyType
  }

  def typeToProtocol(in: xsbti.api.Type): protocol.Type = in match {
    case x: xsbti.api.SimpleType => simpleTypeToProtocol(x)
    case x: xsbti.api.Annotated =>
      protocol.Annotated(typeToProtocol(x.baseType), x.annotations.map(annotationToProtocol))
    case x: xsbti.api.Structure =>
      protocol.Structure(x.parents.map(typeToProtocol), x.declared.map(definitionToProtocol), x.inherited.map(definitionToProtocol))
    case x: xsbti.api.Polymorphic =>
      protocol.Polymorphic(typeToProtocol(x.baseType), x.parameters.map(typeParameterToProtocol))
    case x: xsbti.api.Existential =>
      protocol.Existential(typeToProtocol(x.baseType), x.clause.map(typeParameterToProtocol))
    case x: xsbti.api.Constant => protocol.Constant(typeToProtocol(x.baseType), x.value)
  }

  def annotationArgumentToProtocol(in: xsbti.api.AnnotationArgument): protocol.AnnotationArgument =
    protocol.AnnotationArgument(name = in.name, value = in.value)

  def annotationToProtocol(in: xsbti.api.Annotation): protocol.Annotation =
    protocol.Annotation(base = typeToProtocol(in.base),
      arguments = in.arguments.map(annotationArgumentToProtocol))

  def modifiersToProtocol(in: xsbti.api.Modifiers): protocol.Modifiers =
    protocol.Modifiers(isAbstract = in.isAbstract,
      isOverride = in.isOverride,
      isFinal = in.isFinal,
      isSealed = in.isSealed,
      isImplicit = in.isImplicit,
      isLazy = in.isLazy,
      isMacro = in.isMacro)

  def qualifierToProtocol(in: xsbti.api.Qualifier): protocol.Qualifier = in match {
    case x: xsbti.api.IdQualifier => protocol.IdQualifier(x.value)
    case x: xsbti.api.ThisQualifier => protocol.ThisQualifier
    case x: xsbti.api.Unqualified => protocol.Unqualified
  }

  def accessToProtocol(in: xsbti.api.Access): protocol.Access = in match {
    case _: xsbti.api.Public => protocol.Public
    case x: xsbti.api.Protected => protocol.Protected(qualifierToProtocol(x.qualifier))
    case x: xsbti.api.Private => protocol.Private(qualifierToProtocol(x.qualifier))
  }

  def sourceAPIToProtocol(in: xsbti.api.SourceAPI): protocol.SourceAPI =
    protocol.SourceAPI(packages = in.packages.map(packageAPIToProtocol),
      definitions = in.definitions.map(definitionToProtocol))

  def definitionToProtocol(in: xsbti.api.Definition): protocol.Definition =
    protocol.Definition(name = in.name,
      access = accessToProtocol(in.access),
      modifiers = modifiersToProtocol(in.modifiers),
      annotations = in.annotations.map(annotationToProtocol))

  def packageAPIToProtocol(in: xsbti.api.Package): protocol.Package =
    protocol.Package(name = in.name)

  def settingKeyToProtocolValue[T](key: SettingKey[T], state: State, extracted: Extracted): TaskResult[T] = {
    val serializations = Serializations.extractOpt(state).getOrElse(throw new RuntimeException("state should have serializations on it"))
    val value = extracted.get(key)
    TaskSuccess(BuildValue(value, serializations)(key.key.manifest))
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

}
