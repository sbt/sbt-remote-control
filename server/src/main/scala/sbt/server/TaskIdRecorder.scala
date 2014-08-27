package sbt
package server

import sbt.Task

/** this is the read-only face of TaskIdRecorder which is used outside of ServerExecuteProgress */
trait TaskIdFinder {
  /**
   * guess task ID from the key and/or the current thread.
   * this returns 0 (invalid task ID) if we can't come up with any guess;
   * doesn't return an Option because normally we want to just send the event
   * anyway with a 0 task ID.
   */
  final def bestGuessTaskId(taskIfKnown: Option[Task[_]] = None): Long =
    bestGuessTaskIdOption(taskIfKnown).getOrElse(0L)

  def bestGuessTaskIdOption(taskIfKnown: Option[Task[_]] = None): Option[Long]

  /** just look up the task ID by key, don't use any thread info. */
  def taskId(task: Task[_]): Option[Long]
}

/**
 *  This class actually records tasks registered via the task engine.
 */
class TaskIdRecorder extends TaskIdFinder {
  // This is always modified from the engine thread, and we will
  // add the ID for each task in register() before
  // the corresponding task runs. So while a task thread might
  // get an old map that's missing a newly-registered ID, that
  // task thread should not care about or need to access said
  // newly-registered ID. The thread which needs a task ID
  // should run post-register. In theory, of course. If this
  // theory is wrong not sure what we'll have to do.
  //
  // Note:  This is a single-producer of changed values, with multiple thread consumers.
  //        If that assumption ever changes, this will have to change
  //        from a volatile into an Atomic Reference and use CAS operations
  //        for writing.
  @volatile private var taskIds: Map[Task[_], Long] = Map.empty
  private var nextTaskId = 1L // start with 1 so 0 is invalid

  /** this one is used from the task threads and thus we have to synchronize. */
  private var runningTasks: Set[Task[_]] = Set.empty

  private object taskIdThreadLocal extends ThreadLocal[Long] {
    override def initialValue(): Long = 0
  }

  // This is only ever called from one thread at a time (we assume).
  // NOTE: We should validate this is the case even in the presence of
  //       custom commands.
  def register(task: Task[_]): Unit = {
    if (taskIds.contains(task))
      throw new RuntimeException(s"registered more than once? ${task}")
    // clear system streams of anything which isn't from the new task
    flushSystemStreams()
    taskIds += (task -> nextTaskId)
    nextTaskId += 1
  }

  def unregister(task: Task[_]): Unit = {
    // clear system streams of anything which is from the previous tasks
    flushSystemStreams()
    taskIds -= task
  }

  def clear(): Unit = {
    // clear system streams of anything which is from the previous tasks
    flushSystemStreams()
    taskIds = Map.empty
  }

  // TODO we want to replace this with *requiring* all event senders
  // to know their key, which means we need to pass the task key
  // through to the UIContext and the EventLogger. This can probably
  // be done with a streamsManager plus somehow relating UIContext to
  // the streams, or handling UIContext in a similar way to streams
  // where it's a dummy value replaced by sbt before invoking each
  // task. Exact details TBD and may require sbt ABI break.
  // The problem with this hack is that if a task spawns its
  // own threads, we won't have the task ID.
  def setThreadTask(task: Task[_]): Unit =
    taskId(task) match {
      case Some(id) =>
        // clear system streams of anything which isn't from the new task
        flushSystemStreams()
        taskIdThreadLocal.set(id)
        synchronized {
          runningTasks += task
        }
      case None =>
        throw new RuntimeException(s"Running a task which was never ExecuteProgress#registered? ${task}")
    }

  def clearThreadTask(task: Task[_]): Unit = {
    // clear system streams of anything which is from the new task
    flushSystemStreams()
    taskIdThreadLocal.remove()
    synchronized {
      runningTasks -= task
    }
  }

  override def bestGuessTaskIdOption(taskIfKnown: Option[Task[_]] = None): Option[Long] = {
    taskIfKnown flatMap { key =>
      taskIds.get(key)
    } orElse {
      taskIdThreadLocal.get match {
        // if we don't have anything in the thread local, if we have
        // only one task running we can guess that one.
        case 0L => synchronized {
          if (runningTasks.size == 1)
            Some(taskIds.get(runningTasks.head).getOrElse(throw new RuntimeException("running task has no ID?")))
          else
            None
        }
        case other => Some(other)
      }
    }
  }

  // because these are buffered and then forwarded to log events
  // that get task IDs, we want to flush whenever our bestGuessTaskId
  // might change.
  private def flushSystemStreams(): Unit = {
    System.out.flush()
    System.err.flush()
  }

  override def taskId(task: Task[_]): Option[Long] = {
    taskIds.get(task)
  }
}