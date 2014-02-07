package sbt
package server

/**
 * Represents a cache of the previous build state(s), if they exist.
 *
 * We diff this after actual commands are run to ensure we notify clients of changes in the build structure.
 */
object BuildStructureCache {
  private val buildStructureCache = AttributeKey[protocol.MinimalBuildStructure]("Sbt's server cache")

  def extract(state: State): Option[protocol.MinimalBuildStructure] = state get buildStructureCache
  private def updateImpl(state: State, cache: protocol.MinimalBuildStructure): State = state.put(buildStructureCache, cache)

  def addListener(state: State, listener: SbtClient): State = {
    val serverState = ServerState.extract(state)
    sendBuildStructure(listener, SbtDiscovery.buildStructure(state))
    ServerState.update(state, serverState.addBuildListener(listener))
  }

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

