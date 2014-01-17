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

case class ExecutionRequest(command: String) extends Request
case class ExecutionDone(command: String) extends Event

// Request for the server to send us all events that happen on the sbt server.
case class ListenToEvents() extends Request

case class ListenToBuildChange() extends Request

case class ListenToValue(key: ScopedKey) extends Request 

/** This is a local internal message fired when a client connection is detected
 * to be closed.
 */
case class ClientClosedRequest() extends Request







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

case class TaskStarted(key: ScopedKey) extends Event
// TODO - Send result? no...
case class TaskFinished(key: ScopedKey, success: Boolean) extends Event
