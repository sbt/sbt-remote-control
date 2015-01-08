package sbt.protocol
import scala.collection.immutable
import sbt.serialization._

/**
 * Utilities to track the state implied by a series of events, allowing the events
 *  to be reconstructed as needed. This is a utility used by both client
 *  and server; the client needs to generate the events to go from this
 *  state to no state on disconnect, while the server needs to generate
 *  the events to "catch up" new clients to the current state.
 */

/* FIXME we don't really need to pass the Pickler around for messages anymore
 * because Message is a sealed trait with the writes at the base. Therefore
 * this whole EventWithWrites thing is pointless - get rid of it.
 */
private[sbt] final case class EventWithWrites[E <: Event](event: E)

private[sbt] object EventWithWrites {
  def withWrites[E <: Event](event: E): EventWithWrites[E] =
    EventWithWrites(event)
}

private[sbt] object ImpliedState {
  import scala.language.implicitConversions

  /* FIXME this hack is because we have EventWithWrites for no good reason anymore
   * and SPickler is invariant.
   */
  private implicit def writesForEvent[E <: Event]: SbtPickler[E] = implicitly[SbtPickler[Message]].asInstanceOf[SbtPickler[E]]

  private implicit def writes[E <: Event](event: E): EventWithWrites[E] =
    EventWithWrites.withWrites(event)

  final case class Task(id: Long, key: Option[ScopedKey])
  final case class Execution(id: Long, command: String, client: ClientInfo, tasks: immutable.Map[Long, Task])
  final case class Job(info: BackgroundJobInfo, executionId: Long)

  final case class ExecutionEngine(waiting: immutable.Map[Long, Execution], started: immutable.Map[Long, Execution], jobs: immutable.Seq[Job])

  object ExecutionEngine {
    val empty = ExecutionEngine(Map.empty, Map.empty, Nil)
  }

  private def endExecution(engine: ExecutionEngine, id: Long): ExecutionEngine = {
    if (engine.started.contains(id))
      engine.copy(started = engine.started - id)
    else
      throw new RuntimeException(s"Received end of execution event on execution which wasn't started")
  }

  def processEvent(engine: ExecutionEngine, event: ExecutionWaiting): ExecutionEngine = {
    if (engine.waiting.contains(event.id))
      throw new RuntimeException("got multiple execution creations " + event)
    engine.copy(waiting = engine.waiting + (event.id ->
      Execution(id = event.id, command = event.command, client = event.client, tasks = Map.empty)))
  }

  /**
   * Modify the engine state according to the provided event.
   *  It is a bug to provide an event in the wrong sequence or
   *  with unknown IDs (the events must be correct and in the
   *  correct order).
   */
  def processEvent(engine: ExecutionEngine, event: ExecutionEngineEvent): ExecutionEngine = event match {
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
    case TaskFinished(executionId, taskId, key, success, messageOption) =>
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
    case BackgroundJobStarted(executionId, info) =>
      engine.copy(jobs = Job(info, executionId) +: engine.jobs)
    case BackgroundJobFinished(executionId, jobId) =>
      engine.copy(jobs = engine.jobs.filter(_.info.id != jobId))
    case BuildFailedToLoad() =>
      engine
    case BuildLoaded() =>
      engine
  }

  def processEvents(engine: ExecutionEngine, events: immutable.Seq[Event]): ExecutionEngine =
    events.foldLeft(engine) { (sofar, event) =>
      event match {
        case e: ExecutionWaiting => processEvent(sofar, e)
        case e: ExecutionEngineEvent => processEvent(sofar, e)
        case other => sofar
      }
    }

  private def eventToStartJob(job: Job): EventWithWrites[BackgroundJobStarted] =
    BackgroundJobStarted(executionId = job.executionId, job = job.info)

  private def eventToFinishJob(job: Job): EventWithWrites[BackgroundJobFinished] =
    BackgroundJobFinished(executionId = job.executionId, jobId = job.info.id)

  private def eventToStartTask(executionId: Long, task: Task): EventWithWrites[TaskStarted] =
    TaskStarted(executionId = executionId, taskId = task.id, key = task.key)

  private def eventToFinishTask(executionId: Long, task: Task, success: Boolean): EventWithWrites[TaskFinished] =
    TaskFinished(executionId = executionId, taskId = task.id, key = task.key, success = success,
      message = if (success) None else Some("Disconnected from sbt"))

  private def eventToCreateExecution(execution: Execution): EventWithWrites[ExecutionWaiting] =
    ExecutionWaiting(execution.id, execution.command, execution.client)

  private def eventsToStartExecution(execution: Execution): immutable.Seq[EventWithWrites[_ <: Event]] = {
    val startTasks = for (task <- execution.tasks.values)
      yield eventToStartTask(execution.id, task)

    (Iterable(writes(ExecutionStarting(execution.id))) ++ startTasks).toList
  }

  private def eventsToCreateAndStartExecution(execution: Execution): immutable.Seq[EventWithWrites[_ <: Event]] = {
    (Iterable(eventToCreateExecution(execution)) ++ eventsToStartExecution(execution)).toList
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
    val starting = engine.started.values.flatMap { execution => eventsToCreateAndStartExecution(execution) }
    val waiting = for (execution <- engine.waiting.values)
      yield writes(ExecutionWaiting(execution.id, execution.command, execution.client))
    // it is a slight violation of ideal semantics to start a job outside of the execution
    // the job was started by, but for no-longer-running executions there isn't a lot we can do.
    // Possibly we should fake-start-and-stop the execution around this, but that could cause trouble too.
    // So I think we just need to relax the semantics: job events are not guaranteed to be inside
    // the job-creating execution.
    val jobs = engine.jobs.map { job => eventToStartJob(job) }
    (starting ++ waiting ++ jobs).toList
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
    val finishJobs = engine.jobs.map { job =>
      eventToFinishJob(job)
    }
    (starting ++ waiting ++ finishJobs).toList
  }
}
