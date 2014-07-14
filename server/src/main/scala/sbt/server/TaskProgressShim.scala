package sbt
package server

import protocol.{
  ExecutionEngineEvent,
  TaskStarted,
  TaskFinished,
  ValueChanged,
  TaskResult,
  TaskFailure,
  TaskSuccess,
  BuildValue
}

private[server] class ServerExecuteProgress(state: ServerState, taskIdRecorder: TaskIdRecorder, eventSink: JsonSink[ExecutionEngineEvent]) extends ExecuteProgress[Task] {
  type S = ServerState
  def initial: S = state

  private def taskId(task: Task[_]): Long = {
    taskIdRecorder.taskId(task).getOrElse(throw new RuntimeException(s"Task was not registered? no task ID for $task"))
  }

  private def protocolKeyOption(task: Task[_]): Option[protocol.ScopedKey] =
    task.info.get(Keys.taskDefinitionKey) map {
      SbtToProtocolUtils.scopedKeyToProtocol(_)
    }

  // Note: not all tasks have keys. Anonymous tasks are allowed.
  private def withKeyAndProtocolKey(task: Task[_])(block: (ScopedKey[_], protocol.ScopedKey) => Unit): Unit = {
    task.info.get(Keys.taskDefinitionKey) match {
      case Some(key) =>
        block(key, SbtToProtocolUtils.scopedKeyToProtocol(key))
      case None => // Ignore tasks without keys.
    }
  }

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
    eventSink.send(TaskStarted(state.requiredExecutionId.id,
      taskId(task),
      protocolKeyOption(task)))
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
    eventSink.send(TaskFinished(state.requiredExecutionId.id,
      taskId(task),
      protocolKeyOption(task), result.toEither.isRight))

    taskIdRecorder.unregister(task)

    withKeyAndProtocolKey(task) { (key, protocolKey) =>
      // we want to serialize the value only once, iff there's
      // a listener at all...
      lazy val event = {
        // TODO - Check value against some "last value cache"
        val mf = getManifestOfTask[T](key.key.manifest)
        ValueChanged(protocolKey, resultToProtocol(result, mf))
      }
      for {
        kl <- state.keyListeners
        // these explicit types may look paranoid but I got it wrong twice!
        if (kl.key: sbt.Def.ScopedKey[_]) == (key.scopedKey: sbt.Def.ScopedKey[_])
      } kl.client.send(event)
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
  def getShims(state: State, taskIdRecorder: TaskIdRecorder, eventSink: JsonSink[ExecutionEngineEvent]): Seq[Setting[_]] = {
    Seq(
      Keys.executeProgress in Global := { (state: State) =>
        val sstate = server.ServerState.extract(state)
        new Keys.TaskProgress(new ServerExecuteProgress(sstate, taskIdRecorder, eventSink))
      })

  }
}
