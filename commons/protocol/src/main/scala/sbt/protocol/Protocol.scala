package sbt.protocol

// Note:  All the serialization mechanisms for this protocol is in the
// package.scala file.

/** A marker trait for *any* message that is passed back/forth from
 *  sbt into a client.
 */
sealed trait Message {
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
  def simpleName = removeDollar(lastChunk(getClass.getName))
}
/** Represents requests that go down into sbt. */
sealed trait Request extends Message
/** Responses that come back from sbt. */
sealed trait Response extends Message
/** Events that get sent during requests to sbt. */
sealed trait Event extends Message


// ------------------------------------------
//              Requests (Reactive API)
// ------------------------------------------

case class RegisterClientRequest(uuid: String, configName: String, humanReadableName: String) extends Request

case class ExecutionRequest(command: String) extends Request
// if the request was combined with an identical pending one,
// then the id will be the same for the combined requests.
case class ExecutionRequestReceived(id: Long) extends Response
case class ExecutionDone(id: Long, command: String) extends Event
case class ExecutionFailure(id: Long, command: String) extends Event

/**
 * @param id An identifier we'll receive when we get the list of completions.
 * @param in The (partial) command we'd like possible completions for.
 * @param level  The interpretation of `level` is up to parser definitions, but 0 is the default by convention,
 * with increasing positive numbers corresponding to increasing verbosity.  Typically no more than
 * a few levels are defined. 
 */
case class CommandCompletionsRequest(id: String, in: String, level: Int) extends Request
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
case class CommandCompletionsResponse(id: String, results: Set[Completion]) extends Response

// Request for the server to send us all events that happen on the sbt server.
case class ListenToEvents() extends Request

case class ListenToBuildChange() extends Request

case class ListenToValue(key: ScopedKey) extends Request
// This is issued if a request for a key value fails.
case class KeyNotFound(key: ScopedKey) extends Event 

/** This is a local internal message fired when a client connection is detected
 * to be closed.
 */
case class ClientClosedRequest() extends Request


case class KeyLookupRequest(name: String) extends Request
case class KeyLookupResponse(name: String, key: Seq[ScopedKey]) extends Request

case class KeyListRequest(filter: KeyFilter) extends Request
case class KeyListResponse(keys: KeyList) extends Response




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
/** We have a new log to display. */
case class LogEvent(entry: LogEntry) extends Event
/** exactly one of the boot events is sent on startup */
sealed trait BootEvent extends Event
/** we need to restart sbt in an orderly fashion */
case object NeedRebootEvent extends BootEvent
/** we successfully booted up */
case object NowListeningEvent extends BootEvent
/** The server is shutting down. */
case object Stopped extends Event

/** A build test has done something useful and we're being notified of it. */
case class TestEvent(name: String, description: Option[String], outcome: TestOutcome, error: Option[String]) extends Event
/** A generic mechanism to send events. */
//case class GenericEvent(value: play.api.libs.json.JsValue) extends Event
/** The build has been changed in some fashion. */
case class BuildStructureChanged(structure: MinimalBuildStructure) extends Event
case class ValueChange[T](key: ScopedKey, value: TaskResult[T]) extends Event

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
    msg: String
) extends Event

case class TaskStarted(executionId: Long, key: ScopedKey) extends Event
// TODO - Send result? no...
case class TaskFinished(executionId: Long, key: ScopedKey, success: Boolean) extends Event
