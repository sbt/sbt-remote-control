package sbt
package server

case class LastCommand(command: String, serial: Long, client: LiveClient)
/**
 * Represents the current state of the sbt server we use to drive
 * events/handle client requests.
 */
case class ServerState(
  eventListeners: SbtClient = NullSbtClient,
  buildListeners: SbtClient = NullSbtClient,
  keyListeners: Seq[KeyValueClientListener[_]] = Seq.empty,

  lastCommand: Option[LastCommand] = None) {

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
  def addBuildListener(l: SbtClient): ServerState = {
    val next = buildListeners zip l
    copy(buildListeners = next)
  }
  def withLastCommand(cmd: LastCommand): ServerState = {
    copy(lastCommand = Some(cmd))
  }
  def clearLastCommand: ServerState = copy(lastCommand = None)

  def addKeyListener[T](client: SbtClient, key: ScopedKey[T]): ServerState = {
    // TODO - Speed this up.
    val handler =
      keyListeners.find(_.key == key).getOrElse(KeyValueClientListener(key, NullSbtClient))
    val newListeners = keyListeners.filterNot(_.key == key) :+ handler.add(client)
    copy(keyListeners = newListeners)
  }
}

object ServerState {
  val serverState = AttributeKey[ServerState]("Sbt's server state")

  def extract(state: State): ServerState = Project.getOrError(state, serverState, "Could not find sbt's server state.")
  def update(state: State, serverState: ServerState): State = state.put(ServerState.serverState, serverState)
}