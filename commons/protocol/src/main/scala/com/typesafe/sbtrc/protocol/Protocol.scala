package com.typesafe.sbtrc.protocol

import com.typesafe.sbtrc.ipc
import scala.util.parsing.json._
import com.typesafe.sbtrc.ipc.JsonReader
import java.io.File

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
sealed trait Request extends Message {
  /** whether to send events between request/response, in
   * addition to just the response. 
   */
  def sendEvents: Boolean
}
/** Responses that come back from sbt. */
sealed trait Response extends Message
/** Events that get sent during requests to sbt. */
sealed trait Event extends Message


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

/** when we receive a request but before we process it, we send this */
case object RequestReceivedEvent extends Event
// pseudo-wire-messages we synthesize locally
case object Started extends Event
case object Stopped extends Event
/** If you see this, something very bad has happened. */
case class MysteryMessage(something: Any) extends Event
/** A build test has done somethign useful and we're being notified of it. */
case class TestEvent(name: String, description: Option[String], outcome: TestOutcome, error: Option[String]) extends Event
/** A generic mechanism to send events. */
case class GenericEvent(id: String, params: Map[String, Any]) extends Event


// -----------------------------------------
//    Low-Level API
// -----------------------------------------

/*
 * This represents a low-level "dirty" API that talks directly to sbt's 
 * abstractions.
 */

/**
 * Attempts to cancel the last request this client sent.
 */
case object CancelRequest extends Request {
  override def sendEvents = false
}
case object CancelResponse extends Response


/** can be the response to anything. */
case class ErrorResponse(error: String) extends Response

/** Helper for defining requests to look at sbt keys. */
trait KeyRequest extends Request {
  def filter: KeyFilter
  def sendEvents = false
}
/** Request for all keys that define settings. */
case class SettingKeyRequest(filter: KeyFilter) extends KeyRequest
/** Request for all keys that define tasks. */
case class TaskKeyRequest(filter: KeyFilter) extends KeyRequest
/** Request for all keys that define input tasks. */
case class InputTaskKeyRequest(filter: KeyFilter) extends KeyRequest
/** Request for the value of a setting. */
case class SettingValueRequest(key: ScopedKey) extends Request {
  override val sendEvents = false
}
/** Request for the value resulting from executing a task.
 *  This will execute the task.
 */
case class TaskValueRequest(key: ScopedKey, sendEvents: Boolean = false) extends Request
/** A response that returns the requested keys. */
case class KeyListResponse(keys: KeyList) extends Response 
case class SettingValueResponse[T](value: TaskResult[T]) extends Response
case class TaskValueResponse[T](value: TaskResult[T]) extends Response

case class ExecuteCommandRequest(command: String, sendEvents: Boolean = false) extends Request
case class ExecuteCommandResponse() extends Response



// -----------------------------------------
//    High-Level API
// -----------------------------------------

/*
 * An API that should remain mostly unchanged across sbt versions.
 */

/** Requests project names and general information about the projects. */
case class NameRequest(sendEvents: Boolean) extends Request
case class ProjectInfo(ref: ProjectReference, name: String, default: Boolean = false, attributes: Map[String, Any] = Map.empty)
case class NameResponse(projects: Seq[ProjectInfo]) extends Response


sealed trait RequestOnProject extends Request {
  def ref: Option[ProjectReference]
}
/** Requests the default main class, and known main class for a project (requires a compile). 
 *  
 *  @param ref - An optional project from which we should run detection.
 *               If none is specified, then all are returned.
 */
case class MainClassRequest(sendEvents: Boolean, ref: Option[ProjectReference] = None) extends RequestOnProject
case class MainClassResponse(projects: Seq[DiscoveredMainClasses]) extends Response
case class DiscoveredMainClasses(project: ProjectReference, mainClasses: Seq[String], defaultMainClass: Option[String] = None)


/** Returns the source files that sbt would watch if given a 
 *  Triggered execution command. 
 */
case class WatchTransitiveSourcesRequest(sendEvents: Boolean) extends Request 
case class WatchTransitiveSourcesResponse(files: Seq[File]) extends Response

/**
 * Requests for sbt to compile the project.
 */
case class CompileRequest(sendEvents: Boolean) extends Request 
case class CompileResponse(success: Boolean) extends Response

/**
 * Requests for sbt to run a given main class on a project.
 */
case class RunRequest(sendEvents: Boolean, mainClass: Option[String], useAtmos: Boolean = false) extends Request 
case class RunResponse(success: Boolean, task: String) extends Response 

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
/**
 * Requests for sbt to run the tests for a given project.
 */
case class TestRequest(sendEvents: Boolean) extends Request
case class TestResponse(outcome: TestOutcome) extends Response {
  def success: Boolean = outcome.success
}