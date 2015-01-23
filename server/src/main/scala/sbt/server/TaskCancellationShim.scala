package sbt
package server

/**
 * This is a strategy which can cancel tasks when their associated cancellation `Future[_]`
 * is completed.
 */
final class ServerTaskCancellation(serverState: ServerState, logSink: MessageSink[protocol.LogEvent]) extends TaskCancellationStrategy {
  def debug(s: String): Unit =
    logSink.send(protocol.CoreLogEvent(protocol.LogMessage(protocol.LogMessage.DEBUG, s)))

  def warn(s: String): Unit =
    logSink.send(protocol.CoreLogEvent(protocol.LogMessage(protocol.LogMessage.WARN, s)))

  /* override */ class State(canceller: RunningTaskEngine, val lastCommand: Option[LastCommand]) {
    @volatile
    var enabled = true
    def cancel(): Unit = {
      debug(s"cancel strategy cancel() with enabled=$enabled lastCommand=$lastCommand")
      if (enabled) canceller.cancelAndShutdown()
    }
  }
  override def onTaskEngineStart(canceller: RunningTaskEngine): State = {
    val state = new State(canceller, serverState.lastCommand)
    debug(s"cancel strategy start enabled=${state.enabled} lastCommand=${state.lastCommand}")
    state.lastCommand match {
      case Some(command) =>
        // Note: this callback could be invoked immediately
        // and on our thread - I hope sbt can cope with that...
        command.command.cancelStatus.onCancel { () =>
          state.cancel()
        }
      case None =>
        warn(s"cancel strategy for task engine does not know current command; will not be able to cancel")
    }
    state
  }
  override def onTaskEngineFinish(state: State): Unit = {
    state.enabled = false
    debug(s"cancel strategy finish enabled=${state.enabled} lastCommand=${state.lastCommand}")
  }
}
object ServerTaskCancellation {
  def getShims(logSink: MessageSink[protocol.LogEvent]): Seq[Setting[_]] = {
    Seq(
      Keys.taskCancelStrategy in Global := { (state: State) =>
        val sstate = server.ServerState.extract(state)
        new ServerTaskCancellation(sstate, logSink)
      })

  }
}
