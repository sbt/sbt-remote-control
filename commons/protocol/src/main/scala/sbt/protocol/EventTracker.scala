package sbt.protocol
import scala.collection.immutable
import play.api.libs.json.Writes

/**
 * Utilities to track the state implied by a series of events, allowing the events
 *  to be reconstructed as needed. This is a utility used by both client
 *  and server; the client needs to generate the events to go from this
 *  state to no state on disconnect, while the server needs to generate
 *  the events to "catch up" new clients to the current state.
 */

case class EventWithWrites[E <: Event](event: E, writes: Writes[E])

object ImpliedState {
  import scala.language.implicitConversions

  private implicit def writes[E <: Event, W >: E](event: E)(implicit writes: Writes[W]): EventWithWrites[E] =
    EventWithWrites(event, implicitly[Writes[E]])

  case class Task(id: Long, key: Option[ScopedKey])
  case class Execution(id: Long, command: String, client: ClientInfo, tasks: immutable.Map[Long, Task])

  case class ExecutionEngine(waiting: immutable.Map[Long, Execution], started: immutable.Map[Long, Execution])

  object ExecutionEngine {
    val empty = ExecutionEngine(Map.empty, Map.empty)
  }

  private def endExecution(engine: ExecutionEngine, id: Long): ExecutionEngine = {
    if (engine.started.contains(id))
      engine.copy(started = engine.started - id)
    else
      throw new RuntimeException(s"Received end of execution event on execution which wasn't started")
  }

  /**
   * Modify the engine state according to the provided event.
   *  It is a bug to provide an event in the wrong sequence or
   *  with unknown IDs (the events must be correct and in the
   *  correct order).
   */
  def processEvent(engine: ExecutionEngine, event: Event): ExecutionEngine = event match {
    case ExecutionWaiting(id, command, client) =>
      engine.copy(waiting = engine.waiting + (id ->
        Execution(id = id, command = command, client = client, tasks = Map.empty)))
    case ExecutionStarting(id) =>
      if (engine.waiting.contains(id))
        engine.copy(waiting = engine.waiting - id, started = engine.started + (id -> engine.waiting(id)))
      else
        throw new RuntimeException(s"Received execution starting without a waiting first ${event}")
    case ExecutionSuccess(id) =>
      endExecution(engine, id)
    case ExecutionFailure(id) =>
      endExecution(engine, id)
    case TaskStarted(executionId, taskId, key) =>
      engine.started.get(executionId) match {
        case Some(oldExecution) =>
          val newExecution = oldExecution.copy(tasks = oldExecution.tasks + (taskId -> Task(taskId, key)))
          engine.copy(started = engine.started + (executionId -> newExecution))
        case None =>
          throw new RuntimeException(s"Received TaskStarted for an execution we don't know about")
      }
    case TaskFinished(executionId, taskId, key, success) =>
      engine.started.get(executionId) match {
        case Some(oldExecution) =>
          if (oldExecution.tasks.contains(taskId)) {
            val newExecution = oldExecution.copy(tasks = oldExecution.tasks - taskId)
            engine.copy(started = engine.started + (executionId -> newExecution))
          } else {
            throw new RuntimeException(s"Received TaskFinished for a task we don't know about ${event}")
          }
        case None =>
          throw new RuntimeException(s"Received TaskFinished for an execution we don't know about")
      }
    case _ =>
      engine
  }

  def processEvents(engine: ExecutionEngine, events: immutable.Seq[Event]): ExecutionEngine =
    events.foldLeft(engine) { (sofar, event) => processEvent(sofar, event) }

  private def eventToStartTask(executionId: Long, task: Task): EventWithWrites[TaskStarted] =
    TaskStarted(executionId = executionId, taskId = task.id, key = task.key)

  private def eventToFinishTask(executionId: Long, task: Task, success: Boolean): EventWithWrites[TaskFinished] =
    TaskFinished(executionId = executionId, taskId = task.id, key = task.key, success = success)

  private def eventsToStartExecution(execution: Execution): immutable.Seq[EventWithWrites[_ <: Event]] = {
    val startExecution: Seq[EventWithWrites[_ <: Event]] =
      Seq(writes(ExecutionWaiting(execution.id, execution.command, execution.client)),
        writes(ExecutionStarting(execution.id)))
    val startTasks = for (task <- execution.tasks.values)
      yield eventToStartTask(execution.id, task)

    (startExecution ++ startTasks).toList
  }

  private def eventsToFinishExecution(execution: Execution, success: Boolean): immutable.Seq[EventWithWrites[_ <: Event]] = {
    val finishTasks = for (task <- execution.tasks.values)
      yield eventToFinishTask(execution.id, task, success)
    val finishExecution: EventWithWrites[_ <: Event] =
      if (success)
        writes(ExecutionSuccess(execution.id))
      else
        writes(ExecutionFailure(execution.id))
    (finishTasks ++ Iterable(finishExecution)).toList
  }

  /**
   * Compute a list of events which will create the provided engine state
   * if processed in order from first to last.
   */
  def eventsToReachEngineState(engine: ExecutionEngine): immutable.Seq[EventWithWrites[_ <: Event]] = {
    val starting = engine.started.values.flatMap { execution => eventsToStartExecution(execution) }
    val waiting = for (execution <- engine.waiting.values)
      yield writes(ExecutionWaiting(execution.id, execution.command, execution.client))
    (starting ++ waiting).toList
  }

  /**
   * Compute a list of events which will empty the provided engine state and
   * ifi processed in order from first to last.
   */
  def eventsToEmptyEngineState(engine: ExecutionEngine, success: Boolean): immutable.Seq[EventWithWrites[_ <: Event]] = {
    val starting = engine.started.values.flatMap { execution => eventsToFinishExecution(execution, success) }
    val waiting =
      engine.waiting.values.flatMap { execution =>
        // we have no way to cancel an execution without starting it
        eventsToStartExecution(execution) ++ eventsToFinishExecution(execution, success)
      }
    (starting ++ waiting).toList
  }
}
