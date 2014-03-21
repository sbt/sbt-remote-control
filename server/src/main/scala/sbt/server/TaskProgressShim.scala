package sbt
package server

import protocol.{
  TaskStarted,
  TaskFinished,
  ValueChange,
  TaskResult,
  TaskFailure,
  TaskSuccess,
  BuildValue
}

private[server] class ServerExecuteProgress(state: ServerState) extends ExecuteProgress[Task] {
  type S = ServerState
  def initial: S = state

  /**
   * Notifies that a `task` has been registered in the system for execution.
   * The dependencies of `task` are `allDeps` and the subset of those dependencies that
   * have not completed are `pendingDeps`.
   */
  def registered(state: S, task: Task[_], allDeps: Iterable[Task[_]], pendingDeps: Iterable[Task[_]]): S = state

  /**
   * Notifies that all of the dependencies of `task` have completed and `task` is therefore
   * ready to run.  The task has not been scheduled on a thread yet.
   */
  def ready(state: S, task: Task[_]): S = {
    task.info.get(Keys.taskDefinitionKey) match {
      case Some(key) =>
        // Send notification
        state.eventListeners.send(TaskStarted(state.requiredExecutionId.id,
          SbtToProtocolUtils.scopedKeyToProtocol(key)))
      case None => // Ignore tasks without keys.
    }
    state
  }

  // This is not called on the engine thread, so we can't get state.  For now, we'll ignore it.
  def workStarting(task: Task[_]): Unit = ()
  // This is not called on the engine thread, so we can't have state.  For now, we'll ignore it.
  def workFinished[T](task: Task[T], result: Either[Task[T], Result[T]]): Unit = ()

  /**
   * Notifies that `task` has completed.
   * The task's work is done with a final `result`.
   * Any tasks called by `task` have completed.
   */
  def completed[T](state: S, task: Task[T], result: Result[T]): S = {

    task.info.get(Keys.taskDefinitionKey) match {
      case Some(key) =>
        // Send basic notification
        val protocolKey = SbtToProtocolUtils.scopedKeyToProtocol(key)
        state.eventListeners.send(TaskFinished(state.requiredExecutionId.id, protocolKey, result.toEither.isRight))
        for {
          kl <- state.keyListeners
          if kl.key == key
          // TODO - Check value against some "last value cache"
          mf = getManifestOfTask[T](key.key.manifest)
        } kl.client.send(ValueChange(protocolKey, resultToProtocol(result, mf)))
      case None => // Ignore tasks without keys.
    }
    state
  }

  // Very very dirty hack...
  private def getManifestOfTask[T](mf: Manifest[_]): Manifest[T] = {
    if (mf.erasure == classOf[Task[_]]) {
      mf.typeArguments(0).asInstanceOf[Manifest[T]]
    } else mf.asInstanceOf[Manifest[T]]
  }

  private def resultToProtocol[T](result: Result[T], mf: Manifest[T]): TaskResult[T] = {
    result match {
      case Value(v) => TaskSuccess(BuildValue(v)(mf))
      case Inc(err) => TaskFailure(err.getMessage)
    }
  }

  /** All tasks have completed with the final `results` provided. */
  def allCompleted(state: S, results: RMap[Task, Result]): S = state
}
object ServerExecuteProgress {
  def getShims(state: State): Seq[Setting[_]] = {
    Seq(
      Keys.executeProgress in Global := { (state: State) =>
        val sstate = server.ServerState.extract(state)
        new Keys.TaskProgress(new ServerExecuteProgress(sstate))
      })

  }
}