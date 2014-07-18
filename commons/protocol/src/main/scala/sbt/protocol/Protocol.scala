package sbt.protocol

// Note:  All the serialization mechanisms for this protocol is in the
// package.scala file.

import play.api.libs.json.JsValue

/**
 * A marker trait for *any* message that is passed back/forth from
 *  sbt into a client.
 */
sealed trait Message {
  def simpleName: String = Message.makeSimpleName(getClass)
}

object Message {
  // this makes it prettier when writing json by hand e.g. in JavaScript
  private def removeDollar(s: String) = {
    val i = s.lastIndexOf('$')
    if (i >= 0)
      s.substring(0, i)
    else
      s
  }
  // avoiding class.getSimpleName because apparently it's buggy with some
  // Scala name manglings
  private def lastChunk(s: String) = {
    val i = s.lastIndexOf('.')
    if (i >= 0)
      s.substring(i + 1)
    else
      s
  }
  private[protocol] def makeSimpleName(klass: Class[_]): String =
    removeDollar(lastChunk(klass.getName))
}

/** Represents requests that go down into sbt. */
sealed trait Request extends Message
/** Responses that come back from sbt. */
sealed trait Response extends Message
/** Events that get sent during requests to sbt. */
sealed trait Event extends Message
/** Events sent by the execution engine */
sealed trait ExecutionEngineEvent extends Event

// ------------------------------------------
//              Requests (Reactive API)
// ------------------------------------------

case class ClientInfo(uuid: String, configName: String, humanReadableName: String)

case class RegisterClientRequest(info: ClientInfo) extends Request

case class CancelExecutionRequest(id: Long) extends Request
case class CancelExecutionResponse(attempted: Boolean) extends Response

case class ExecutionRequest(command: String) extends Request
case class KeyExecutionRequest(key: ScopedKey) extends Request
// if the request was combined with an identical pending one,
// then the id will be the same for the combined requests.
case class ExecutionRequestReceived(id: Long) extends Response
// execution queued up
case class ExecutionWaiting(id: Long, command: String, client: ClientInfo) extends Event
// about to execute this one (popped off the queue)
case class ExecutionStarting(id: Long) extends ExecutionEngineEvent
// finished executing successfully
case class ExecutionSuccess(id: Long) extends ExecutionEngineEvent
// finished executing unsuccessfully
case class ExecutionFailure(id: Long) extends ExecutionEngineEvent

/**
 * Request for the server to completely shut down.  No response expected,
 * as this is equivalent to issuing a kill -9.
 */
case class KillServerRequest() extends Request

/**
 * @param in The (partial) command we'd like possible completions for.
 * @param level  The interpretation of `level` is up to parser definitions, but 0 is the default by convention,
 * with increasing positive numbers corresponding to increasing verbosity.  Typically no more than
 * a few levels are defined.
 */
case class CommandCompletionsRequest(in: String, level: Int) extends Request
/**
 * Represents a completion.
 * The abstract members `display` and `append` are best explained with an example.
 *
 * Assuming space-delimited tokens, processing this:
 *   am is are w<TAB>
 * could produce these Completions:
 *   Completion { display = "was"; append = "as" }
 *   Completion { display = "were"; append = "ere" }
 * to suggest the tokens "was" and "were".
 *
 * In this way, two pieces of information are preserved:
 *  1) what needs to be appended to the current input if a completion is selected
 *  2) the full token being completed, which is useful for presenting a user with choices to select
 */
case class Completion(append: String, display: String, isEmpty: Boolean)
case class CommandCompletionsResponse(results: Set[Completion]) extends Response

// Request for the server to send us all events that happen on the sbt server.
case class ListenToEvents() extends Request
case class UnlistenToEvents() extends Request

case class ListenToBuildChange() extends Request
case class UnlistenToBuildChange() extends Request
// send us a build changed event even if it didn't change
case class SendSyntheticBuildChanged() extends Request

case class ListenToValue(key: ScopedKey) extends Request
case class UnlistenToValue(key: ScopedKey) extends Request
// send us a value changed event even if it didn't change
case class SendSyntheticValueChanged(key: ScopedKey) extends Request
// This is issued if a request for a key value fails.
case class KeyNotFound(key: ScopedKey) extends Response

/**
 * This is fired as an implementation detail on server side when a client connection is detected
 * to be closed. TODO having this in public API is sort of terrible.
 */
case class ClientClosedRequest() extends Request

/**
 * This is synthesized client-side when a client connection closes. It
 *  purposely has no Format since it doesn't go over the wire.
 */
case class ClosedEvent() extends Event

case class KeyLookupRequest(name: String) extends Request
case class KeyLookupResponse(name: String, key: Seq[ScopedKey]) extends Response

// -----------------------------------------
//                  Events
// -----------------------------------------

/*
 * Events may happen at any time during a request/response cycle.  These
 * represent things that occur during the processing of requests.
 */

sealed trait LogEntry {
  def message: String
}
case class LogStdOut(message: String) extends LogEntry
case class LogStdErr(message: String) extends LogEntry
case class LogSuccess(message: String) extends LogEntry
case class LogTrace(throwableClass: String, message: String) extends LogEntry
case class LogMessage(level: String, message: String) extends LogEntry {
  if (!LogMessage.validLevels.contains(level))
    throw new RuntimeException("Not a valid log level: '" + level + "'")
}

object LogMessage {
  val DEBUG = "debug"
  val INFO = "info"
  val WARN = "warn"
  val ERROR = "error"
  private[protocol] val validLevels = Set(DEBUG, INFO, WARN, ERROR)
}
/** We have a new log to display. taskId is 0 if the task is unknown. */
case class LogEvent(taskId: Long, entry: LogEntry) extends Event

/** A custom event from a task. "name" is conventionally the simplified class name. */
case class TaskEvent(taskId: Long, name: String, serialized: JsValue) extends Event

object TaskEvent {
  import play.api.libs.json.Writes

  def apply[T: Writes](taskId: Long, event: T): TaskEvent = {
    val json = implicitly[Writes[T]].writes(event)
    TaskEvent(taskId, Message.makeSimpleName(event.getClass), json)
  }
}

/** Companion objects of events which can go in a task event extend this */
trait TaskEventUnapply[T] {
  import play.api.libs.json.Reads
  import scala.reflect.ClassTag
  import play.api.libs.json.Json

  def unapply(event: Event)(implicit reads: Reads[T], classTag: ClassTag[T]): Option[(Long, T)] = event match {
    case taskEvent: TaskEvent =>
      val name = Message.makeSimpleName(implicitly[ClassTag[T]].runtimeClass)
      if (name != taskEvent.name) {
        None
      } else {
        Json.fromJson[T](taskEvent.serialized).asOpt map { result => taskEvent.taskId -> result }
      }
    case other => None
  }
}

/** The build has been changed in some fashion. */
case class BuildStructureChanged(structure: MinimalBuildStructure) extends Event
case class ValueChanged[T](key: ScopedKey, value: TaskResult[T]) extends Event

/** can be the response to anything. */
case class ErrorResponse(error: String) extends Response
/** A notification that a given request has been received. */
case class ReceivedResponse() extends Response
case class RequestCompleted() extends Response
case class RequestFailed() extends Response

case class ReadLineRequest(executionId: Long, prompt: String, mask: Boolean) extends Request
case class ReadLineResponse(line: Option[String]) extends Response
case class ConfirmRequest(executionId: Long, message: String) extends Request
case class ConfirmResponse(confirmed: Boolean) extends Response

// the taskId is provided here (tying it to an executionId and key),
// and then in further events from the task we only provide taskId
// since the exeuctionId and key can be deduced from that.
case class TaskStarted(executionId: Long, taskId: Long, key: Option[ScopedKey]) extends ExecutionEngineEvent
// we really could provide taskId ONLY here, but we throw the executionId and key
// in just for convenience so clients don't have to hash taskId if their
// only interest is in the key and executionId
case class TaskFinished(executionId: Long, taskId: Long, key: Option[ScopedKey], success: Boolean) extends ExecutionEngineEvent

///// Events below here are intended to go inside a TaskEvent

/** A build test has done something useful and we're being notified of it. */
case class TestEvent(name: String, description: Option[String], outcome: TestOutcome, error: Option[String])

object TestEvent extends TaskEventUnapply[TestEvent]

sealed trait TestOutcome {
  final def success: Boolean = {
    this != TestError && this != TestFailed
  }

  final def combine(other: TestOutcome): TestOutcome = {
    // this same logic is used to compute an overall result in sbt.TestEvent
    if (other == TestError || this == TestError)
      TestError
    else if (other == TestFailed || this == TestFailed)
      TestFailed
    else if (other == TestPassed || this == TestPassed)
      TestPassed
    else
      TestSkipped
  }
}

object TestOutcome {
  def apply(s: String): TestOutcome = s match {
    case "passed" => TestPassed
    case "failed" => TestFailed
    case "error" => TestError
    case "skipped" => TestSkipped
  }
}

case object TestPassed extends TestOutcome {
  override def toString = "passed"
}
case object TestFailed extends TestOutcome {
  override def toString = "failed"
}
case object TestError extends TestOutcome {
  override def toString = "error"
}
case object TestSkipped extends TestOutcome {
  override def toString = "skipped"
}

/** A compilation issue from the compiler. */
case class CompilationFailure(
  project: ProjectReference,
  position: xsbti.Position,
  severity: xsbti.Severity,
  message: String)

object CompilationFailure extends TaskEventUnapply[CompilationFailure]
