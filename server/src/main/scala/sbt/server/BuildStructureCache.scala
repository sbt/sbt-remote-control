package sbt
package server

import sbt.serialization._

/**
 * Represents a cache of the previous build state(s), if they exist.
 *
 * We diff this after actual commands are run to ensure we notify clients of changes in the build structure.
 */
object BuildStructureCache {
  private val buildStructureCache = AttributeKey[protocol.MinimalBuildStructure]("Sbt's server cache")

  // TODO - ideally we use this mechanism to also track Setting listeners.

  def extract(state: State): Option[protocol.MinimalBuildStructure] = state get buildStructureCache
  private def updateImpl(state: State, cache: protocol.MinimalBuildStructure): State = state.put(buildStructureCache, cache)

  def update(state: State): State = {
    val structure = SbtDiscovery.buildStructure(state)
    extract(state) match {
      case Some(previous) if previous == structure => state
      case _ =>
        val serverState = ServerState.extract(state)
        sendBuildStructure(serverState.buildListeners, structure)
        updateImpl(state, structure)
    }
  }

  def sendBuildStructure(listener: SbtClient, structure: protocol.MinimalBuildStructure): Unit =
    listener.send(protocol.BuildStructureChanged(structure))
}

