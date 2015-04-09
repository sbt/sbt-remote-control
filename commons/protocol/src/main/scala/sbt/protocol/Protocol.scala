package sbt.protocol

import java.io.File
import scala.collection.immutable
import sbt.serialization._

/**
 * A marker trait for *any* message that is passed back/forth from
 *  sbt into a client.
 */
@directSubclasses(Array(classOf[Request], classOf[Response], classOf[Event], classOf[UnknownMessage]))
sealed trait Message

/** Represents requests that go down into sbt. */
@directSubclasses(Array(classOf[RegisterClientRequest],
  classOf[DaemonRequest],
  classOf[CancelExecutionRequest],
  classOf[ExecutionRequest],
  classOf[KeyExecutionRequest],
  classOf[KillServerRequest],
  classOf[CommandCompletionsRequest],
  classOf[ListenToEvents],
  classOf[UnlistenToEvents],
  classOf[ListenToBuildChange],
  classOf[UnlistenToBuildChange],
  classOf[SendSyntheticBuildChanged],
  classOf[ListenToValue],
  classOf[UnlistenToValue],
  classOf[SendSyntheticValueChanged],
  classOf[ClientClosedRequest],
  classOf[KeyLookupRequest],
  classOf[AnalyzeExecutionRequest],
  classOf[ReadLineRequest],
  classOf[ConfirmRequest]))
sealed trait Request extends Message
/** Responses that come back from sbt. */
@directSubclasses(Array(
  classOf[RegisterClientResponse],
  classOf[CancelExecutionResponse],
  classOf[ExecutionRequestReceived],
  classOf[CommandCompletionsResponse],
  classOf[KeyNotFound],
  classOf[KeyLookupResponse],
  classOf[AnalyzeExecutionResponse],
  classOf[ErrorResponse],
  classOf[ReceivedResponse],
  classOf[ReadLineResponse],
  classOf[ConfirmResponse]))
sealed trait Response extends Message
/** Events that get sent during requests to sbt. */
@directSubclasses(Array(classOf[ExecutionEngineEvent],
  classOf[ExecutionWaiting],
  classOf[ClosedEvent],
  classOf[LogEvent],
  classOf[PluginEvent],
  classOf[BuildStructureChanged],
  classOf[ValueChanged]))
sealed trait Event extends Message
/** Events sent by the execution engine */
@directSubclasses(Array(classOf[ExecutionStarting],
  classOf[ExecutionSuccess],
  classOf[ExecutionFailure],
  classOf[BuildLoaded],
  classOf[BuildFailedToLoad],
  classOf[TaskStarted],
  classOf[TaskFinished],
  classOf[BackgroundJobStarted],
  classOf[BackgroundJobFinished]))
sealed trait ExecutionEngineEvent extends Event

// ------------------------------------------
//              Requests (Reactive API)
// ------------------------------------------

// "protocolVersion" doesn't have semantics like major.minor;
// versions are supposed to be backward compatible always (if they
// weren't we'd need to do something more radical than send this,
// pre-connection, such as change the sbt server launch
// procedure). It's probably easier to use FeatureTag for most
// "negotiation" scenarios, too. But you could also have a rule
// like "in protocol version 3, if both sides are >= 3 you do
// xyz."
sealed abstract class ProtocolVersion protected (val name: String) {
  final override def toString = name
  final override def equals(x: Any): Boolean = x match {
    case null => false
    case other: ProtocolVersion => name == other.name
    case _ => false
  }
  final override def hashCode(): Int = name.hashCode
}
object ProtocolVersion {
  private implicit val resultToString = CanToString[ProtocolVersion](_.toString,
    {
      case "1" => ProtocolVersion1
      case "2" => ProtocolVersion2
      // we don't want to explode here if we get a version
      // we don't know. If it's handled somehow, it'd be
      // elsewhere (in the client or server logic).
      case other => ProtocolVersionUnknown
    })

  implicit val picklerUnpickler: Pickler[ProtocolVersion] with Unpickler[ProtocolVersion] =
    canToStringPickler[ProtocolVersion]

  implicit object ordering extends Ordering[ProtocolVersion] {
    override def compare(x: ProtocolVersion, y: ProtocolVersion): Int =
      (x, y) match {
        case (x, y) if x == y => 0
        case (ProtocolVersionUnknown, _) => -1
        case (_, ProtocolVersionUnknown) => 1
        // TODO - We may need real comparision here...
        case (x, y) => x.name.compareTo(y.name)
      }
  }
}

case object ProtocolVersionUnknown extends ProtocolVersion("unknown")
case object ProtocolVersion1 extends ProtocolVersion("1")
case object ProtocolVersion2 extends ProtocolVersion("2")

// These enable or disable certain behaviors. For example you might have
// a tag like "SupportsFoo" and if a client sets that tag the server
// could enable "Foo", or whatever.
sealed abstract class FeatureTag protected (val name: String) {
  final override def toString = name
  final override def equals(x: Any): Boolean = x match {
    case null => false
    case other: FeatureTag => name == other.name
    case _ => false
  }
  final override def hashCode(): Int = name.hashCode
}
object FeatureTag {
  private implicit val resultToString = CanToString[FeatureTag](_.toString,
    {
      // tags are extensible, so it's fine to get some we don't understand
      // and ignore them.
      case other => FeatureTagUnknown
    })

  implicit val picklerUnpickler: Pickler[FeatureTag] with Unpickler[FeatureTag] =
    canToStringPickler[FeatureTag]
}

case object FeatureTagUnknown extends FeatureTag("unknown")

final case class ServerInfo(protocolVersion: ProtocolVersion, featureTags: Vector[FeatureTag])
final case class ClientInfo(uuid: String, configName: String, humanReadableName: String, protocolVersion: ProtocolVersion, featureTags: Vector[FeatureTag])

final case class RegisterClientRequest(info: ClientInfo) extends Request
final case class RegisterClientResponse(info: ServerInfo) extends Response

/**
 * If there's a request or event we don't understand because it's from
 * a newer version of the protocol, then we put it here. Unknown responses
 * are converted to ErrorResponse and generally aren't "allowed" because
 * it's not backward compatible to change the possible responses to an
 * existing request, so this will be a request or event.
 * This message is never sent over the wire.
 */
final case class UnknownMessage(serialized: SerializedValue) extends Message

/** whether the client should prevent the server from exiting */
final case class DaemonRequest(daemon: Boolean) extends Request

final case class CancelExecutionRequest(id: Long) extends Request
final case class CancelExecutionResponse(attempted: Boolean) extends Response

final case class ExecutionRequest(command: String) extends Request
final case class KeyExecutionRequest(key: ScopedKey) extends Request
// if the request was combined with an identical pending one,
// then the id will be the same for the combined requests.
final case class ExecutionRequestReceived(id: Long) extends Response
// execution queued up
final case class ExecutionWaiting(id: Long, command: String, client: ClientInfo) extends Event
// about to execute this one (popped off the queue)
final case class ExecutionStarting(id: Long) extends ExecutionEngineEvent
// finished executing successfully
final case class ExecutionSuccess(id: Long) extends ExecutionEngineEvent
// finished executing unsuccessfully
final case class ExecutionFailure(id: Long) extends ExecutionEngineEvent

/**
 * Request for the server to completely shut down.  No response expected,
 * as this is equivalent to issuing a kill -9.
 */
final case class KillServerRequest() extends Request

/**
 * @param in The (partial) command we'd like possible completions for.
 * @param level  The interpretation of `level` is up to parser definitions, but 0 is the default by convention,
 * with increasing positive numbers corresponding to increasing verbosity.  Typically no more than
 * a few levels are defined.
 */
final case class CommandCompletionsRequest(in: String, level: Int) extends Request
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
final case class Completion(append: String, display: String, isEmpty: Boolean)
final case class CommandCompletionsResponse(results: Vector[Completion]) extends Response

// Request for the server to send us all events that happen on the sbt server.
final case class ListenToEvents() extends Request
final case class UnlistenToEvents() extends Request

final case class ListenToBuildChange() extends Request
final case class UnlistenToBuildChange() extends Request
// send us a build changed event even if it didn't change
final case class SendSyntheticBuildChanged() extends Request

final case class ListenToValue(key: ScopedKey) extends Request
final case class UnlistenToValue(key: ScopedKey) extends Request
// send us a value changed event even if it didn't change
final case class SendSyntheticValueChanged(key: ScopedKey) extends Request
// This is issued if a request for a key value fails.
final case class KeyNotFound(key: ScopedKey) extends Response

/**
 * This is fired as an implementation detail on server side when a client connection is detected
 * to be closed. TODO having this in public API is sort of terrible.
 */
final case class ClientClosedRequest() extends Request

/**
 * This is synthesized client-side when a client connection closes. It
 *  purposely has no Format since it doesn't go over the wire.
 */
final case class ClosedEvent() extends Event

final case class KeyLookupRequest(name: String) extends Request
final case class KeyLookupResponse(name: String, key: Vector[ScopedKey]) extends Response

final case class AnalyzeExecutionRequest(command: String) extends Request

@directSubclasses(Array(classOf[ExecutionAnalysisKey], classOf[ExecutionAnalysisError],
  classOf[ExecutionAnalysisCommand]))
sealed trait ExecutionAnalysis
// sbt will run ALL of these keys (aggregation)
final case class ExecutionAnalysisKey(keys: Vector[ScopedKey]) extends ExecutionAnalysis
final case class ExecutionAnalysisError(message: String) extends ExecutionAnalysis
final case class ExecutionAnalysisCommand(name: Option[String]) extends ExecutionAnalysis
final case class AnalyzeExecutionResponse(analysis: ExecutionAnalysis) extends Response

// -----------------------------------------
//                  Events
// -----------------------------------------

/*
 * Events may happen at any time during a request/response cycle.  These
 * represent things that occur during the processing of requests.
 */

// knownDirectSubclasses doesn't seem to come out right for LogEntry,
// but only nondeterministically (one compile will work, another won't),
// so be very careful about removing this.
@directSubclasses(Array(classOf[LogStdOut], classOf[LogStdErr], classOf[LogSuccess],
  classOf[LogTrace], classOf[LogMessage]))
sealed trait LogEntry {
  def message: String
}
final case class LogStdOut(message: String) extends LogEntry
final case class LogStdErr(message: String) extends LogEntry
final case class LogSuccess(message: String) extends LogEntry
final case class LogTrace(throwableClass: String, message: String) extends LogEntry
final case class LogMessage(level: String, message: String) extends LogEntry {
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
@directSubclasses(Array(classOf[TaskLogEvent],
  classOf[DetachedLogEvent],
  classOf[BackgroundJobLogEvent]))
sealed trait LogEvent extends Event {
  def entry: LogEntry
}
/** A log event from a task (marked with task ID) */
final case class TaskLogEvent(taskId: Long, entry: LogEntry) extends LogEvent {
  require(taskId != 0L)
}
/** A log event not attached to a task or job (no task ID or job ID available) */
final case class DetachedLogEvent(entry: LogEntry) extends LogEvent

/** A log event from a background job */
final case class BackgroundJobLogEvent(jobId: Long, entry: LogEntry) extends LogEvent {
  require(jobId != 0L)
}

/**
 * An event sent by a plugin, which we pass through to clients.
 */
@directSubclasses(Array(classOf[DetachedEvent],
  classOf[TaskEvent],
  classOf[BackgroundJobEvent]))
sealed trait PluginEvent extends Event {
  def serialized: SerializedValue
}

/** A custom event with no task or job ID. */
final case class DetachedEvent(serialized: SerializedValue) extends PluginEvent

object DetachedEvent {
  def apply[T: Pickler](event: T): DetachedEvent = {
    DetachedEvent(SerializedValue(event))
  }
}

/** Companion objects of events that can go in a DetachedEvent extend this */
trait DetachedEventUnapply[T] {
  def unapply(event: Event)(implicit unpickler: Unpickler[T]): Option[T] = event match {
    case detachedEvent: DetachedEvent =>
      if (detachedEvent.serialized.hasTag[T])
        Some(detachedEvent.serialized.parse[T].get)
      else
        None
    case other => None
  }
}

/** A custom event from a task. */
final case class TaskEvent(taskId: Long, serialized: SerializedValue) extends PluginEvent

object TaskEvent {
  def apply[T: Pickler](taskId: Long, event: T): TaskEvent = {
    val serialized = SerializedValue(event)
    TaskEvent(taskId, serialized)
  }
}

/** Companion objects of events which can go in a task event extend this */
trait TaskEventUnapply[T] {
  def unapply(event: Event)(implicit unpickler: Unpickler[T]): Option[(Long, T)] = event match {
    case taskEvent: TaskEvent =>
      if (taskEvent.serialized.hasTag[T])
        Some(taskEvent.taskId -> taskEvent.serialized.parse[T].get)
      else
        None
    case other => None
  }
}

/** A custom event from a job. */
final case class BackgroundJobEvent(jobId: Long, serialized: SerializedValue) extends PluginEvent

object BackgroundJobEvent {
  def apply[T: Pickler](jobId: Long, event: T): BackgroundJobEvent = {
    val serialized = SerializedValue(event)
    BackgroundJobEvent(jobId, serialized)
  }
}

/** Companion objects of events which can go in a task event extend this */
trait BackgroundJobEventUnapply[T] {
  def unapply(event: Event)(implicit unpickler: Unpickler[T]): Option[(Long, T)] = event match {
    case jobEvent: BackgroundJobEvent =>
      if (jobEvent.serialized.hasTag[T])
        Some(jobEvent.jobId -> jobEvent.serialized.parse[T].get)
      else
        None
    case other => None
  }
}

/** Build has been loaded or reloaded successfully. Typically followed by a BuildStructureChanged. */
final case class BuildLoaded() extends ExecutionEngineEvent
/** Build has failed to load or reload. */
final case class BuildFailedToLoad() extends ExecutionEngineEvent

/** The build has been changed in some fashion. */
final case class BuildStructureChanged(structure: MinimalBuildStructure) extends Event
final case class ValueChanged(key: ScopedKey, value: TaskResult) extends Event

/** can be the response to anything. */
final case class ErrorResponse(error: String) extends Response
/** A notification that a given request has been received. */
final case class ReceivedResponse() extends Response

final case class ReadLineRequest(executionId: Long, prompt: String, mask: Boolean) extends Request
final case class ReadLineResponse(line: Option[String]) extends Response
final case class ConfirmRequest(executionId: Long, message: String) extends Request
final case class ConfirmResponse(confirmed: Boolean) extends Response

// the taskId is provided here (tying it to an executionId and key),
// and then in further events from the task we only provide taskId
// since the executionId and key can be deduced from that.
final case class TaskStarted(executionId: Long, taskId: Long, key: Option[ScopedKey]) extends ExecutionEngineEvent
// we really could provide taskId ONLY here, but we throw the executionId and key
// in just for convenience so clients don't have to hash taskId if their
// only interest is in the key and executionId. Also we include the error
// message so you can get it even if you don't watch ValueChanged.
final case class TaskFinished(executionId: Long, taskId: Long, key: Option[ScopedKey], success: Boolean, message: Option[String]) extends ExecutionEngineEvent

final case class BackgroundJobInfo(id: Long, humanReadableName: String, spawningTask: ScopedKey)

final case class BackgroundJobStarted(executionId: Long, job: BackgroundJobInfo) extends ExecutionEngineEvent
final case class BackgroundJobFinished(executionId: Long, jobId: Long) extends ExecutionEngineEvent

// TODO currently due to a pickling bug caused by a Scala bug,
// the macros won't know all the subtypes of Message if we
// put this companion object earlier in the file.
object Message {

  // These various picklers are mostly alphabetical except when
  // they have to be sorted in dependency order.

  // Picklers for types that appear in messages

  private implicit val backgroundJobInfoPickler = genPickler[BackgroundJobInfo]
  private implicit val backgroundJobInfoUnpickler = genUnpickler[BackgroundJobInfo]
  private implicit val clientInfoPickler = genPickler[ClientInfo]
  private implicit val clientInfoUnpickler = genUnpickler[ClientInfo]
  private implicit val serverInfoPickler = genPickler[ServerInfo]
  private implicit val serverInfoUnpickler = genUnpickler[ServerInfo]
  private implicit val completionPickler = genPickler[Completion]
  private implicit val completionUnpickler = genUnpickler[Completion]
  private implicit val executionAnalysisCommandPickler = genPickler[ExecutionAnalysisCommand]
  private implicit val executionAnalysisCommandUnpickler = genUnpickler[ExecutionAnalysisCommand]
  private implicit val executionAnalysisErrorPickler = genPickler[ExecutionAnalysisError]
  private implicit val executionAnalysisErrorUnpickler = genUnpickler[ExecutionAnalysisError]
  private implicit val executionAnalysisKeyPickler = genPickler[ExecutionAnalysisKey]
  private implicit val executionAnalysisKeyUnpickler = genUnpickler[ExecutionAnalysisKey]
  private implicit val executionAnalysisPickler = genPickler[ExecutionAnalysis]
  private implicit val executionAnalysisUnpickler = genUnpickler[ExecutionAnalysis]
  private implicit val logStdErrPickler = genPickler[LogStdErr]
  private implicit val logStdErrUnpickler = genUnpickler[LogStdErr]
  private implicit val logStdOutPickler = genPickler[LogStdOut]
  private implicit val logStdOutUnpickler = genUnpickler[LogStdOut]
  private implicit val logSuccessPickler = genPickler[LogSuccess]
  private implicit val logSuccessUnpickler = genUnpickler[LogSuccess]
  private implicit val logTracePickler = genPickler[LogTrace]
  private implicit val logTraceUnpickler = genUnpickler[LogTrace]
  private implicit val logMessagePickler = genPickler[LogMessage]
  private implicit val logMessageUnpickler = genUnpickler[LogMessage]
  private implicit val logEntryPickler = genPickler[LogEntry]
  private implicit val logEntryUnpickler = genUnpickler[LogEntry]

  // We have PRIVATE implicit picklers for all the leaf subtypes of
  // Message, and then we have a public pickler for the entire Message
  // trait.
  private implicit val analyzeExecutionRequestPickler = genPickler[AnalyzeExecutionRequest]
  private implicit val analyzeExecutionRequestUnpickler = genUnpickler[AnalyzeExecutionRequest]
  private implicit val analyzeExecutionResponsePickler = genPickler[AnalyzeExecutionResponse]
  private implicit val analyzeExecutionResponseUnpickler = genUnpickler[AnalyzeExecutionResponse]
  private implicit val backgroundJobEventPickler = genPickler[BackgroundJobEvent]
  private implicit val backgroundJobEventUnpickler = genUnpickler[BackgroundJobEvent]
  private implicit val backgroundJobFinishedPickler = genPickler[BackgroundJobFinished]
  private implicit val backgroundJobFinishedUnpickler = genUnpickler[BackgroundJobFinished]
  private implicit val backgroundJobLogEventPickler = genPickler[BackgroundJobLogEvent]
  private implicit val backgroundJobLogEventUnpickler = genUnpickler[BackgroundJobLogEvent]
  private implicit val backgroundJobStartedPickler = genPickler[BackgroundJobStarted]
  private implicit val backgroundJobStartedUnpickler = genUnpickler[BackgroundJobStarted]
  private implicit val buildFailedToLoadPickler = genPickler[BuildFailedToLoad]
  private implicit val buildFailedToLoadUnpickler = genUnpickler[BuildFailedToLoad]
  private implicit val buildLoadedPickler = genPickler[BuildLoaded]
  private implicit val buildLoadedUnpickler = genUnpickler[BuildLoaded]
  private implicit val buildStructureChangedPickler = genPickler[BuildStructureChanged]
  private implicit val buildStructureChangedUnpickler = genUnpickler[BuildStructureChanged]
  private implicit val cancelExecutionRequestPickler = genPickler[CancelExecutionRequest]
  private implicit val cancelExecutionRequestUnpickler = genUnpickler[CancelExecutionRequest]
  private implicit val cancelExecutionResponsePickler = genPickler[CancelExecutionResponse]
  private implicit val cancelExecutionResponseUnpickler = genUnpickler[CancelExecutionResponse]
  private implicit val clientClosedRequestPickler = genPickler[ClientClosedRequest]
  private implicit val clientClosedRequestUnpickler = genUnpickler[ClientClosedRequest]
  private implicit val closedEventPickler = genPickler[ClosedEvent]
  private implicit val closedEventUnpickler = genUnpickler[ClosedEvent]
  private implicit val commandCompletionsRequestPickler = genPickler[CommandCompletionsRequest]
  private implicit val commandCompletionsRequestUnpickler = genUnpickler[CommandCompletionsRequest]
  private implicit val commandCompletionsResponsePickler = genPickler[CommandCompletionsResponse]
  private implicit val commandCompletionsResponseUnpickler = genUnpickler[CommandCompletionsResponse]
  private implicit val confirmRequestPickler = genPickler[ConfirmRequest]
  private implicit val confirmRequestUnpickler = genUnpickler[ConfirmRequest]
  private implicit val confirmResponsePickler = genPickler[ConfirmResponse]
  private implicit val confirmResponseUnpickler = genUnpickler[ConfirmResponse]
  private implicit val detachedEventPickler = PicklerUnpickler.generate[DetachedEvent]
  private implicit val detachedLogEventPickler = genPickler[DetachedLogEvent]
  private implicit val detachedLogEventUnpickler = genUnpickler[DetachedLogEvent]
  private implicit val errorResponsePickler = genPickler[ErrorResponse]
  private implicit val errorResponseUnpickler = genUnpickler[ErrorResponse]
  private implicit val executionFailurePickler = genPickler[ExecutionFailure]
  private implicit val executionFailureUnpickler = genUnpickler[ExecutionFailure]
  private implicit val executionRequestPickler = genPickler[ExecutionRequest]
  private implicit val executionRequestReceivedPickler = genPickler[ExecutionRequestReceived]
  private implicit val executionRequestReceivedUnpickler = genUnpickler[ExecutionRequestReceived]
  private implicit val executionRequestUnpickler = genUnpickler[ExecutionRequest]
  private implicit val executionStartingPickler = genPickler[ExecutionStarting]
  private implicit val executionStartingUnpickler = genUnpickler[ExecutionStarting]
  private implicit val executionSuccessPickler = genPickler[ExecutionSuccess]
  private implicit val executionSuccessUnpickler = genUnpickler[ExecutionSuccess]
  private implicit val executionWaitingPickler = genPickler[ExecutionWaiting]
  private implicit val executionWaitingUnpickler = genUnpickler[ExecutionWaiting]
  private implicit val keyExecutionRequestPickler = genPickler[KeyExecutionRequest]
  private implicit val keyExecutionRequestUnpickler = genUnpickler[KeyExecutionRequest]
  private implicit val keyLookupRequestPickler = genPickler[KeyLookupRequest]
  private implicit val keyLookupRequestUnpickler = genUnpickler[KeyLookupRequest]
  private implicit val keyLookupResponsePickler = genPickler[KeyLookupResponse]
  private implicit val keyLookupResponseUnpickler = genUnpickler[KeyLookupResponse]
  private implicit val keyNotFoundPickler = genPickler[KeyNotFound]
  private implicit val keyNotFoundUnpickler = genUnpickler[KeyNotFound]
  private implicit val killServerRequestPickler = genPickler[KillServerRequest]
  private implicit val killServerRequestUnpickler = genUnpickler[KillServerRequest]
  private implicit val listenToBuildChangePickler = genPickler[ListenToBuildChange]
  private implicit val listenToBuildChangeUnpickler = genUnpickler[ListenToBuildChange]
  private implicit val listenToEventsPickler = genPickler[ListenToEvents]
  private implicit val listenToEventsUnpickler = genUnpickler[ListenToEvents]
  private implicit val listenToValuePickler = genPickler[ListenToValue]
  private implicit val listenToValueUnpickler = genUnpickler[ListenToValue]
  private implicit val taskLogEventPickler = genPickler[TaskLogEvent]
  private implicit val taskLogEventUnpickler = genUnpickler[TaskLogEvent]
  private implicit val logEventPickler = genPickler[LogEvent]
  private implicit val logEventUnpickler = genUnpickler[LogEvent]
  private implicit val taskEventPickler = PicklerUnpickler.generate[TaskEvent]
  private implicit val pluginEventPickler = PicklerUnpickler.generate[PluginEvent]
  private implicit val readLineRequestPickler = genPickler[ReadLineRequest]
  private implicit val readLineRequestUnpickler = genUnpickler[ReadLineRequest]
  private implicit val readLineResponsePickler = genPickler[ReadLineResponse]
  private implicit val readLineResponseUnpickler = genUnpickler[ReadLineResponse]
  private implicit val receivedResponsePickler = genPickler[ReceivedResponse]
  private implicit val receivedResponseUnpickler = genUnpickler[ReceivedResponse]
  private implicit val registerClientRequestPickler = genPickler[RegisterClientRequest]
  private implicit val registerClientRequestUnpickler = genUnpickler[RegisterClientRequest]
  private implicit val registerClientResponsePickler = PicklerUnpickler.generate[RegisterClientResponse]
  private implicit val sendSyntheticBuildChangedPickler = genPickler[SendSyntheticBuildChanged]
  private implicit val sendSyntheticBuildChangedUnpickler = genUnpickler[SendSyntheticBuildChanged]
  private implicit val sendSyntheticValueChangedPickler = genPickler[SendSyntheticValueChanged]
  private implicit val sendSyntheticValueChangedUnpickler = genUnpickler[SendSyntheticValueChanged]
  private implicit val daemonRequestPickler = genPickler[DaemonRequest]
  private implicit val daemonRequestUnpickler = genUnpickler[DaemonRequest]
  private implicit val taskFinishedPickler = genPickler[TaskFinished]
  private implicit val taskFinishedUnpickler = genUnpickler[TaskFinished]
  private implicit val taskStartedPickler = genPickler[TaskStarted]
  private implicit val taskStartedUnpickler = genUnpickler[TaskStarted]
  private implicit val testEventPickler = genPickler[TestEvent]
  private implicit val testEventUnpickler = genUnpickler[TestEvent]
  private implicit val unlistenToBuildChangePickler = genPickler[UnlistenToBuildChange]
  private implicit val unlistenToBuildChangeUnpickler = genUnpickler[UnlistenToBuildChange]
  private implicit val unlistenToEventsPickler = genPickler[UnlistenToEvents]
  private implicit val unlistenToEventsUnpickler = genUnpickler[UnlistenToEvents]
  private implicit val unlistenToValuePickler = genPickler[UnlistenToValue]
  private implicit val unlistenToValueUnpickler = genUnpickler[UnlistenToValue]
  private implicit val valueChangedPickler = genPickler[ValueChanged]
  private implicit val valueChangedUnpickler = genUnpickler[ValueChanged]
  private implicit val unknownMessagePickler = PicklerUnpickler.generate[UnknownMessage]

  private implicit val requestPickler = genPickler[Request]
  private implicit val requestUnpickler = genUnpickler[Request]
  private implicit val responsePickler = genPickler[Response]
  private implicit val responseUnpickler = genUnpickler[Response]
  private implicit val executionEngineEventPickler = genPickler[ExecutionEngineEvent]
  private implicit val executionEngineEventUnpickler = genUnpickler[ExecutionEngineEvent]
  private implicit val eventPickler = genPickler[Event]
  private implicit val eventUnpickler = genUnpickler[Event]

  implicit val pickler: Pickler[Message] = genPickler[Message]
  implicit val unpickler: Unpickler[Message] = genUnpickler[Message]
}
