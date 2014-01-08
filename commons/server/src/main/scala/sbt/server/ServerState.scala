package sbt
package server

/**
 * Represents the current state of the sbt server we use to drive
 * events/handle client requests.
 */
case class ServerState(
  eventListeners: SbtClient = NullSbtClient,
  buildListeners: SbtClient = NullSbtClient,
  keyListeners: Seq[KeyValueClientListener[_]] = Seq.empty) {

  /** Remove a client from any registered listeners. */
  def disconnect(client: SbtClient): ServerState =
    copy(
      eventListeners = eventListeners without client,
      buildListeners = buildListeners without client,
      keyListeners = keyListeners map (_ disconnect client))
      
  def addEventListener(l: SbtClient): ServerState = {
    val next = eventListeners zip l
    // TODO - This should probably happen in the command
    // engine before running a command.
    EventLogger.updateClient(next)
    copy(eventListeners = next)
  }
}

object ServerState {
  val serverState = AttributeKey[ServerState]("Sbt's server state")
  
  def extract(state: State): ServerState = Project.getOrError(state, serverState, "Could not find sbt's server state.")
  def update(state: State, serverState: ServerState): State = state.put(ServerState.serverState, serverState)
}