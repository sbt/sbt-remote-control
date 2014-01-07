package sbt

import com.typesafe.sbtrc.server._
import com.typesafe.sbtrc.protocol._

object SimpleServerEngine extends ((SbtServerState, ClientRequest) => SbtServerState) {

  def apply(state: SbtServerState, request: ClientRequest): SbtServerState = {
    val ClientRequest(client, serial, msg) = request
    runRequestImpl(client, serial, msg, state)
  }

  // TODO - Clean this up a bit so it's more obvious what happens.  This method is the
  // core behind our engine.  take in a new request, and feed back the next state.
  def runRequestImpl(client: SbtClient, serial: Long, msg: Request, state: SbtServerState): SbtServerState = msg match {
    case ClientClosedRequest() =>
      // TODO - remove all listeners and state from this client, not just event listeners.
      state.disconnect(client)
    case ListenToEvents() =>
      System.out.println("Registering listener: " + client)
      state.copy(eventListeners = state.eventListeners zip client)
    // TODO - return ACK
    case ExecutionRequest(command) =>
      System.out.println("Handling request for: " + command)
      val next = executeCommand(state, command)
      // Notify original listeners.
      state.eventListeners.send(ExecutionDone(command))
      next
    case _ => state
  }

  def executeCommand(state: SbtServerState, command: String): SbtServerState = {
    state
  }

  /**
   * Executes something against state with a new global logger.
   *
   *  NOTE: Copied from MainLoop with slightly different interface...
   */
  private def withNewLog[A](state: State, logBacking: GlobalLogBacking)(run: State => A): A =
    Using.fileWriter(append = true)(logBacking.file) { writer =>
      val out = new java.io.PrintWriter(writer)
      val newLogging = state.globalLogging.newLogger(out, logBacking)
      transferLevels(state, newLogging)
      val loggedState = state.copy(globalLogging = newLogging)
      try run(loggedState)
      finally out.close()
    }

  /** Transfers logging and trace levels from the old global loggers to the new ones. */
  private[this] def transferLevels(state: State, logging: GlobalLogging) {
    val old = state.globalLogging
    Logger.transferLevels(old.backed, logging.backed)
    (old.full, logging.full) match { // well, this is a hack
      case (oldLog: AbstractLogger, newLog: AbstractLogger) => Logger.transferLevels(oldLog, newLog)
      case _ => ()
    }
  }

}