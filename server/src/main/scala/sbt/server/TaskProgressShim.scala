package sbt
package server

import protocol.{
  TaskStarted,
  TaskFinished,
  ValueChanged,
  TaskResult,
  TaskFailure,
  TaskSuccess,
  BuildValue
}

private[server] class ServerExecuteProgress(state: ServerState, taskIdRecorder: TaskIdRecorder) extends ExecuteProgress[Task] {
  type S = ServerState
  def initial: S = state

  private def taskId(task: Task[_]): Long = {
    taskIdRecorder.taskId(task).getOrElse(throw new RuntimeException(s"Task was not registered? no task ID for $task"))
  }

  // Note: not all tasks have keys. Anonymous tasks are allowed.
  private def withKeyAndProtocolKey(task: Task[_])(block: (ScopedKey[_], protocol.ScopedKey) => Unit): Unit = {
    task.info.get(Keys.taskDefinitionKey) match {
      case Some(key) =>
        block(key, SbtToProtocolUtils.scopedKeyToProtocol(key))
      case None => // Ignore tasks without keys.
    }
  }

  private def withProtocolKey(task: Task[_])(block: protocol.ScopedKey => Unit): Unit =
    withKeyAndProtocolKey(task) { (_, protocolKey) => block(protocolKey) }

  /**
   * Notifies that a `task` has been registered in the system for execution.
   * The dependencies of `task` are `allDeps` and the subset of those dependencies that
   * have not completed are `pendingDeps`. This is called once per task so we expect to
   * get a lot of these calls.
   *
   * Registration can happen throughout execution (as each task
   * runs, it can trigger additional tasks, which will be registered at that time).
   * Duplicate *keys* can be registered, but this code assumes duplicate tasks
   * will not be and my reading of the sbt code is that they won't be.
   */
  def registered(state: S, task: Task[_], allDeps: Iterable[Task[_]], pendingDeps: Iterable[Task[_]]): S = {
    // generate task ID for this one
    taskIdRecorder.register(task)
    state
  }

  /**
   * Notifies that all of the dependencies of `task` have completed and `task` is therefore
   * ready to run.  The task has not been scheduled on a thread yet.
   */
  def ready(state: S, task: Task[_]): S = {
    withProtocolKey(task) { protocolKey =>
      // TODO if a task has no protocol key, we should probably still send
      // an event ...
      state.eventListeners.send(TaskStarted(state.requiredExecutionId.id,
        taskId(task),
        protocolKey))
    }
    state
  }

  // This is not called on the engine thread, so we can't get state.
  def workStarting(task: Task[_]): Unit = {
    taskIdRecorder.setThreadTask(task)
  }
  // This is not called on the engine thread, so we can't have state.
  def workFinished[T](task: Task[T], result: Either[Task[T], Result[T]]): Unit = {
    taskIdRecorder.clearThreadTask(task)
  }

  /**
   * Notifies that `task` has completed.
   * The task's work is done with a final `result`.
   * Any tasks called by `task` have completed.
   */
  def completed[T](state: S, task: Task[T], result: Result[T]): S = {
    // TODO if a task has no protocol key, we should maybe still send an event
    withKeyAndProtocolKey(task) { (key, protocolKey) =>
      state.eventListeners.send(TaskFinished(state.requiredExecutionId.id,
        taskId(task),
        protocolKey, result.toEither.isRight))
      for {
        kl <- state.keyListeners
        if kl.key == key
        // TODO - Check value against some "last value cache"
        mf = getManifestOfTask[T](key.key.manifest)
      } kl.client.send(ValueChanged(protocolKey, resultToProtocol(result, mf)))
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
  def allCompleted(state: S, results: RMap[Task, Result]): S = {
    taskIdRecorder.clear()
    state
  }
}
object ServerExecuteProgress {
  def getShims(state: State, taskIdRecorder: TaskIdRecorder): Seq[Setting[_]] = {
    Seq(
      Keys.executeProgress in Global := { (state: State) =>
        val sstate = server.ServerState.extract(state)
        new Keys.TaskProgress(new ServerExecuteProgress(sstate, taskIdRecorder))
      })

  }
}