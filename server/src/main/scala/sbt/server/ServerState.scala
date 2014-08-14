package sbt
package server

case class LastCommand(command: CommandExecutionWork)

/**
 * Represents the current state of ServerEngine, as passed between
 * commands (ServerState gets tacked on to sbt's State).
 */
final case class ServerState(
  requestListeners: RequestListeners,
  lastCommand: Option[LastCommand] = None) {

  def buildListeners: SbtClient = requestListeners.buildListeners

  def keyListeners: Seq[KeyValueClientListener[_]] = requestListeners.keyListeners

  def withLastCommand(cmd: LastCommand): ServerState = {
    copy(lastCommand = Some(cmd))
  }
  def clearLastCommand: ServerState = copy(lastCommand = None)

  def optionalExecutionId: Option[ExecutionId] = lastCommand.map(_.command.id)
  def requiredExecutionId: ExecutionId = optionalExecutionId.getOrElse(throw new RuntimeException("Last command with execution ID should be set here but is not"))
}

object ServerState {
  val serverState = AttributeKey[ServerState]("Sbt's server state")

  def extract(state: State): ServerState = Project.getOrError(state, serverState, "Could not find sbt's server state.")
  def extractOpt(state: State): Option[ServerState] = state get serverState
  def update(state: State, serverState: ServerState): State = state.put(ServerState.serverState, serverState)
}
