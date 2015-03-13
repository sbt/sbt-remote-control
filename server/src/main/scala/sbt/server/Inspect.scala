package sbt
package server

import sbt.protocol._
import sbt.State
import sbt.Def

object Inspect {
  def listSettingsResponse(state: State): ListSettingsResponse = {
    val extracted = sbt.Project.extract(state)
    val settings =
      for (setting <- extracted.structure.settings)
        yield SbtToProtocolUtils.scopedKeyToProtocol(setting.key)

    ListSettingsResponse(settings.toVector)
  }

  def inspectResponse(state: State, request: InspectRequest): Response = {
    val extracted = sbt.Project.extract(state)
    val structure = extracted.structure

    val scopedOption = SbtToProtocolUtils.protocolToScopedKey(request.key, state)
    scopedOption map { scoped =>
      inspectResponse(state, structure, scoped, preanalyze = request.preanalyze)
    } getOrElse {
      ErrorResponse(s"no such key ${request.key}")
    }
  }

  private def derivedDependencies(comp: Map[sbt.ScopedKey[_], sbt.Def.Compiled[_]], scoped: sbt.ScopedKey[_]): Set[sbt.ScopedKey[_]] =
    comp.get(scoped).map(_.settings.flatMap(s => if (s.isDerived) s.dependencies else Nil)).toList.flatten.toSet

  private def reverseDependencies(cMap: Map[sbt.ScopedKey[_], sbt.Def.Flattened], scoped: sbt.ScopedKey[_]): Iterable[sbt.ScopedKey[_]] =
    for ((key, compiled) <- cMap; dep <- compiled.dependencies if dep == scoped) yield key

  private def inspectResponse(state: State, structure: sbt.BuildStructure, scoped: sbt.ScopedKey[_], preanalyze: Boolean): InspectResponse = {
    def keySeqToProto(keys: Iterable[sbt.ScopedKey[_]]): Vector[sbt.protocol.ScopedKey] =
      keys.map(SbtToProtocolUtils.scopedKeyToProtocol(_)).toVector

    val display = sbt.Project.showContextKey(state)
    val key = scoped.key

    val definingScope = structure.data.definingScope(scoped.scope, key)
    val providedBy = definingScope match {
      case Some(sc) =>
        scoped.copy(scope = sc)
      case None =>
        scoped
    }
    val comp = Def.compiled(structure.settings, actual = true)(structure.delegates, structure.scopeLocal, display)
    val definedAt = ((comp get providedBy).toSeq flatMap { c => c.settings.map(_.pos) }).distinct.toVector

    val cMap = Def.flattenLocals(comp)

    val depends = cMap.get(scoped) match { case Some(c) => c.dependencies.toSet; case None => Set.empty }
    val derivedDepends: Set[sbt.ScopedKey[_]] = derivedDependencies(comp, providedBy)

    val preanalysisOption = if (preanalyze) {
      val reverse = reverseDependencies(cMap, scoped)
      val derivedReverse = reverse.filter(r => derivedDependencies(comp, r).contains(providedBy)).toSet
      val delegates = sbt.Project.delegates(structure, scoped.scope, key)
      val related = cMap.keys.filter(k => k.key == key && k.scope != scoped.scope)

      val preanalysis = InspectPreanalysis(reverseDependencies = keySeqToProto(reverse),
        reverseDerivedDependencies = keySeqToProto(derivedReverse),
        delegates = keySeqToProto(delegates),
        related = keySeqToProto(related))
      Some(preanalysis)
    } else {
      None
    }

    InspectResponse(description = key.description,
      providedBy = SbtToProtocolUtils.scopedKeyToProtocol(providedBy),
      definedAt = definedAt.map(SbtToProtocolUtils.sourcePositionToProtocol(_)),
      dependencies = keySeqToProto(depends),
      derivedDependencies = keySeqToProto(derivedDepends),
      preanalysis = preanalysisOption)
  }
}
