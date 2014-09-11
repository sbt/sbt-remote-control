package sbt.protocol

// Note:  All the serialization mechanisms for this protocol is in the
// package.scala file.

import play.api.libs.json.JsValue
import java.io.File
import scala.collection.immutable

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

final case class ClientInfo(uuid: String, configName: String, humanReadableName: String)

final case class RegisterClientRequest(info: ClientInfo) extends Request

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
final case class CommandCompletionsResponse(results: Set[Completion]) extends Response

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
final case class KeyLookupResponse(name: String, key: Seq[ScopedKey]) extends Response

final case class AnalyzeExecutionRequest(command: String) extends Request
sealed trait ExecutionAnalysis
// sbt will run ALL of these keys (aggregation)
final case class ExecutionAnalysisKey(keys: Seq[ScopedKey]) extends ExecutionAnalysis
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
sealed trait LogEvent extends Event {
  def entry: LogEntry
}
/** A log event from a task (marked with task ID) */
final case class TaskLogEvent(taskId: Long, entry: LogEntry) extends LogEvent {
  require(taskId != 0L)
}
/** A log event from "sbt core" (i.e. not from a task, no task ID available) */
final case class CoreLogEvent(entry: LogEntry) extends LogEvent

/** A log event from a background job */
final case class BackgroundJobLogEvent(jobId: Long, entry: LogEntry) extends LogEvent {
  require(jobId != 0L)
}

/** A custom event from a task. "name" is conventionally the simplified class name. */
final case class TaskEvent(taskId: Long, name: String, serialized: JsValue) extends Event

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

/** A custom event from a task. "name" is conventionally the simplified class name. */
final case class BackgroundJobEvent(jobId: Long, name: String, serialized: JsValue) extends Event

object BackgroundJobEvent {
  import play.api.libs.json.Writes

  def apply[T: Writes](jobId: Long, event: T): BackgroundJobEvent = {
    val json = implicitly[Writes[T]].writes(event)
    BackgroundJobEvent(jobId, Message.makeSimpleName(event.getClass), json)
  }
}

/** Companion objects of events which can go in a task event extend this */
trait BackgroundJobEventUnapply[T] {
  import play.api.libs.json.Reads
  import scala.reflect.ClassTag
  import play.api.libs.json.Json

  def unapply(event: Event)(implicit reads: Reads[T], classTag: ClassTag[T]): Option[(Long, T)] = event match {
    case jobEvent: BackgroundJobEvent =>
      val name = Message.makeSimpleName(implicitly[ClassTag[T]].runtimeClass)
      if (name != jobEvent.name) {
        None
      } else {
        Json.fromJson[T](jobEvent.serialized).asOpt map { result => jobEvent.jobId -> result }
      }
    case other => None
  }
}

/** Build has been loaded or reloaded successfully. Typically followed by a BuildStructureChanged. */
final case class BuildLoaded() extends ExecutionEngineEvent
/** Build has failed to load or reload. */
final case class BuildFailedToLoad() extends ExecutionEngineEvent

/** The build has been changed in some fashion. */
final case class BuildStructureChanged(structure: MinimalBuildStructure) extends Event
final case class ValueChanged[+T, +E <: Throwable](key: ScopedKey, value: TaskResult[T, E]) extends Event

/** can be the response to anything. */
final case class ErrorResponse(error: String) extends Response
/** A notification that a given request has been received. */
final case class ReceivedResponse() extends Response
final case class RequestCompleted() extends Response
final case class RequestFailed() extends Response

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
// only interest is in the key and executionId
final case class TaskFinished(executionId: Long, taskId: Long, key: Option[ScopedKey], success: Boolean) extends ExecutionEngineEvent

final case class BackgroundJobInfo(id: Long, humanReadableName: String, spawningTask: ScopedKey)

final case class BackgroundJobStarted(executionId: Long, job: BackgroundJobInfo) extends ExecutionEngineEvent
final case class BackgroundJobFinished(executionId: Long, jobId: Long) extends ExecutionEngineEvent

///// Events below here are intended to go inside a TaskEvent

final case class TestGroupStarted(name: String)
object TestGroupStarted extends TaskEventUnapply[TestGroupStarted]
final case class TestGroupFinished(name: String, result: TestGroupResult, error: Option[String])
object TestGroupFinished extends TaskEventUnapply[TestGroupFinished]

sealed trait TestGroupResult {
  final def success: Boolean = this == TestGroupPassed
}
case object TestGroupPassed extends TestGroupResult {
  override def toString = "passed"
}
case object TestGroupFailed extends TestGroupResult {
  override def toString = "failed"
}
case object TestGroupError extends TestGroupResult {
  override def toString = "error"
}

/** A build test has done something useful and we're being notified of it. */
final case class TestEvent(name: String, description: Option[String], outcome: TestOutcome, error: Option[String], duration: Long)

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
final case class CompilationFailure(
  project: ProjectReference,
  position: xsbti.Position,
  severity: xsbti.Severity,
  message: String)

object CompilationFailure extends TaskEventUnapply[CompilationFailure]

/* Results related to compilation.
 * TODO let's put all of these in some kind of namespace because
 * there are a bunch of super-generic names like Analysis, API, Package, etc.
 */

final case class Analysis(stamps: Stamps,
  apis: APIs,
  relations: Relations,
  infos: SourceInfos,
  compilations: Compilations)
object Analysis {
  val empty: Analysis = Analysis(stamps = Stamps.empty,
    apis = APIs.empty,
    relations = Relations.empty,
    infos = SourceInfos.empty,
    compilations = Compilations.empty)
}
sealed trait Stamp
final case class Hash(value: ByteArray) extends Stamp
final case class LastModified(value: Long) extends Stamp
final case class Exists(value: Boolean) extends Stamp
final case class Stamps(allInternalSources: Set[File],
  allBinaries: Set[File],
  allProducts: Set[File],
  sources: Map[File, Stamp],
  binaries: Map[File, Stamp],
  products: Map[File, Stamp],
  classNames: Map[File, String])
object Stamps {
  val empty: Stamps = Stamps(allInternalSources = Set.empty[File],
    allBinaries = Set.empty[File],
    allProducts = Set.empty[File],
    sources = Map.empty[File, Stamp],
    binaries = Map.empty[File, Stamp],
    products = Map.empty[File, Stamp],
    classNames = Map.empty[File, String])
}
final case class SourceInfo(reportedProblems: Seq[xsbti.Problem],
  unreportedProblems: Seq[xsbti.Problem])
final case class SourceInfos(allInfos: Map[File, SourceInfo])
object SourceInfos {
  val empty: SourceInfos = SourceInfos(allInfos = Map.empty[File, SourceInfo])
}
final case class Problem(category: String,
  severity: xsbti.Severity,
  message: String,
  position: xsbti.Position) extends xsbti.Problem
object Problem {
  def fromXsbtiProblem(in: xsbti.Problem): Problem =
    Problem(category = in.category,
      severity = in.severity,
      message = in.message,
      position = in.position)
}
final case class APIs(allExternals: Set[String],
  allInternalSources: Set[File],
  internal: Map[File, Source],
  external: Map[String, Source])
object APIs {
  def empty: APIs = APIs(allExternals = Set.empty[String],
    allInternalSources = Set.empty[File],
    internal = Map.empty[File, Source],
    external = Map.empty[String, Source])
}
// TODO this is broken on case-insensitive filesystems
final case class Package(name: String)
sealed trait Qualifier
final case class IdQualifier(value: String) extends Qualifier
final case object ThisQualifier extends Qualifier
final case object Unqualified extends Qualifier
sealed trait Access
sealed trait Qualified extends Access {
  def qualifier: Qualifier
}
final case object Public extends Access
final case class Protected(qualifier: Qualifier) extends Qualified
final case class Private(qualifier: Qualifier) extends Qualified
final case class TypeParameter(id: String, annotations: Seq[Annotation], typeParameters: Seq[TypeParameter], variance: xsbti.api.Variance, lowerBound: Type, upperBound: Type)
sealed trait PathComponent
final case class Id(id: String) extends PathComponent
final case class Super(qualifier: Path) extends PathComponent
final case object This extends PathComponent
final case class Path(components: Seq[PathComponent])
sealed trait Type
object Type {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import play.api.data.validation.ValidationError
  import JsonHelpers._

}
sealed trait SimpleType extends Type
final case class Singleton(path: Path) extends SimpleType
final case class Projection(prefix: SimpleType, id: String) extends SimpleType
final case class Parameterized(baseType: SimpleType, typeArguments: Seq[Type]) extends SimpleType
final case class ParameterRef(id: String) extends SimpleType
final case object EmptyType extends SimpleType
final case class Annotated(baseType: Type, annotations: Seq[Annotation]) extends Type
final case class Structure(parents: Seq[Type], declared: Seq[Definition], inherited: Seq[Definition]) extends Type
final case class Polymorphic(baseType: Type, parameters: Seq[TypeParameter]) extends Type
final case class Existential(baseType: Type, clause: Seq[TypeParameter]) extends Type
final case class Constant(baseType: Type, value: String) extends Type
final case class Modifiers(isAbstract: Boolean,
  isOverride: Boolean,
  isFinal: Boolean,
  isSealed: Boolean,
  isImplicit: Boolean,
  isLazy: Boolean,
  isMacro: Boolean)
final case class AnnotationArgument(name: String, value: String)
final case class Annotation(base: Type,
  arguments: Seq[AnnotationArgument])
final case class Definition(name: String,
  access: Access,
  modifiers: Modifiers,
  annotations: Seq[Annotation])
final case class SourceAPI(packages: Seq[Package],
  definitions: Seq[Definition])
final case class Source(compilation: Compilation,
  hash: ByteArray,
  api: SourceAPI,
  apiHash: Int,
  hasMacro: Boolean)
final case class Relation[A, B](forwardMap: Map[A, Set[B]],
  reverseMap: Map[B, Set[A]])
object Relation {
  def empty[A, B]: Relation[A, B] = Relation[A, B](forwardMap = Map.empty[A, Set[B]],
    reverseMap = Map.empty[B, Set[A]])
}
final case class RelationsSource(internal: Relation[File, File],
  external: Relation[File, String])
final case class Relations(allSources: Set[File],
  allProducts: Set[File],
  allBinaryDeps: Set[File],
  allInternalSrcDeps: Set[File],
  allExternalDeps: Set[String],
  srcProd: Relation[File, File],
  binaryDep: Relation[File, File],
  internalSrcDep: Relation[File, File],
  externalDep: Relation[File, String],
  direct: Option[RelationsSource],
  publicInherited: Option[RelationsSource],
  classes: Relation[File, String])
object Relations {
  val empty: Relations = Relations(allSources = Set.empty[File],
    allProducts = Set.empty[File],
    allBinaryDeps = Set.empty[File],
    allInternalSrcDeps = Set.empty[File],
    allExternalDeps = Set.empty[String],
    srcProd = Relation.empty[File, File],
    binaryDep = Relation.empty[File, File],
    internalSrcDep = Relation.empty[File, File],
    externalDep = Relation.empty[File, String],
    direct = None,
    publicInherited = None,
    classes = Relation.empty[File, String])
}
final case class OutputSetting(sourceDirectory: String,
  outputDirectory: String)
final case class Compilation(startTime: Long,
  outputs: Seq[OutputSetting])
final case class Compilations(allCompilations: Seq[Compilation])
object Compilations {
  val empty: Compilations = Compilations(allCompilations = Seq.empty[Compilation])
}

final class CompileFailedException(message: String, cause: Throwable, val problems: Seq[xsbti.Problem]) extends Exception(message, cause)

sealed trait ByteArray extends immutable.Seq[Byte]
object ByteArray {
  private final val p = 16777619
  private final val start = 2166136261L
  private final def fnvHash(in: Array[Byte]): Int = {
    var hash: Long = start
    var i = 0
    while (i < in.length) {
      hash = (hash ^ in(i)) * p
      i += 1
    }
    hash += hash << 13
    hash ^= hash >> 7
    hash += hash << 3
    hash ^= hash >> 17
    hash += hash << 5
    hash.intValue
  }
  final private class ConcreteByteArray(val ary: Array[Byte]) extends ByteArray {
    private final lazy val hc = fnvHash(ary)
    final def apply(idx: Int): Byte = ary(idx)
    final def iterator: Iterator[Byte] = Iterator.tabulate(ary.length)(i => ary(i))
    final def length: Int = ary.length
    override final def hashCode(): Int = hc
    override final def equals(other: Any): Boolean = other match {
      case x: ConcreteByteArray => java.util.Arrays.equals(ary, x.ary)
      case _ => false
    }
  }
  def apply(in: Array[Byte]): ByteArray = new ConcreteByteArray(in.clone())
}
