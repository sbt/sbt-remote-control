package sbt.protocol

import java.io.File
import scala.collection.immutable
import sbt.serialization._
import scala.pickling.directSubclasses

/**
 * A marker trait for *any* message that is passed back/forth from
 *  sbt into a client.
 */
sealed trait Message {
  def simpleName: String = MessageSerialization.makeSimpleName(getClass)
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
final case class TaskEvent(taskId: Long, name: String, serialized: SerializedValue) extends Event

object TaskEvent {
  def apply[T: SbtPickler](taskId: Long, event: T): TaskEvent = {
    val serialized = JsonValue(event)
    TaskEvent(taskId, MessageSerialization.makeSimpleName(event.getClass), serialized)
  }
}

/** Companion objects of events which can go in a task event extend this */
trait TaskEventUnapply[T] {
  import scala.reflect.ClassTag

  def unapply(event: Event)(implicit unpickler: SbtUnpickler[T], classTag: ClassTag[T]): Option[(Long, T)] = event match {
    case taskEvent: TaskEvent =>
      val name = MessageSerialization.makeSimpleName(implicitly[ClassTag[T]].runtimeClass)
      if (name != taskEvent.name) {
        None
      } else {
        taskEvent.serialized.parse[T].toOption map { result => taskEvent.taskId -> result }
      }
    case other => None
  }
}

/** A custom event from a job. "name" is conventionally the simplified class name. */
final case class BackgroundJobEvent(jobId: Long, name: String, serialized: SerializedValue) extends Event

object BackgroundJobEvent {
  def apply[T: SbtPickler](jobId: Long, event: T): BackgroundJobEvent = {
    val serialized = JsonValue(event)
    BackgroundJobEvent(jobId, MessageSerialization.makeSimpleName(event.getClass), serialized)
  }
}

/** Companion objects of events which can go in a task event extend this */
trait BackgroundJobEventUnapply[T] {
  import scala.reflect.ClassTag

  def unapply(event: Event)(implicit unpickler: SbtUnpickler[T], classTag: ClassTag[T]): Option[(Long, T)] = event match {
    case jobEvent: BackgroundJobEvent =>
      val name = MessageSerialization.makeSimpleName(implicitly[ClassTag[T]].runtimeClass)
      if (name != jobEvent.name) {
        None
      } else {
        jobEvent.serialized.parse[T].toOption map { result => jobEvent.jobId -> result }
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

///// Events below here are intended to go inside a TaskEvent

final case class TestGroupStarted(name: String)
object TestGroupStarted extends TaskEventUnapply[TestGroupStarted] {
  import scala.pickling.{ SPickler, Unpickler, AllPicklers }
  import scala.pickling.static._
  implicit val pickler: SPickler[TestGroupStarted] = genPickler[TestGroupStarted]
  implicit val unpickler: Unpickler[TestGroupStarted] = genUnpickler[TestGroupStarted]
}
final case class TestGroupFinished(name: String, result: TestGroupResult, error: Option[String])
object TestGroupFinished extends TaskEventUnapply[TestGroupFinished] {
  import scala.pickling.{ SPickler, Unpickler, AllPicklers }
  import scala.pickling.static._
  implicit val pickler: SPickler[TestGroupFinished] = genPickler[TestGroupFinished]
  implicit val unpickler: Unpickler[TestGroupFinished] = genUnpickler[TestGroupFinished]
}

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
object TestGroupResult {
  import scala.pickling.{ SPickler, Unpickler, PicklingException }
  import scala.pickling.static._
  import sbt.pickling.CanToString

  private implicit val resultToString = CanToString[TestGroupResult](_.toString,
    {
      case "passed" => TestGroupPassed
      case "failed" => TestGroupFailed
      case "error" => TestGroupError
      case other => throw new PicklingException(s"Unrecognized TestGroupResult $other")
    })

  implicit val picklerUnpickler: SPickler[TestGroupResult] with Unpickler[TestGroupResult] =
    canToStringPickler[TestGroupResult]
}

/** A build test has done something useful and we're being notified of it. */
final case class TestEvent(name: String, description: Option[String], outcome: TestOutcome, error: Option[String], duration: Long)

object TestEvent extends TaskEventUnapply[TestEvent] {
  import scala.pickling.{ SPickler, Unpickler }
  import scala.pickling.static._
  implicit val pickler: SPickler[TestEvent] = genPickler[TestEvent]
  implicit val unpickler: Unpickler[TestEvent] = genUnpickler[TestEvent]
}

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

object TestOutcome {
  import scala.pickling.{ SPickler, Unpickler, PicklingException }
  import scala.pickling.static._
  import sbt.pickling.CanToString

  private implicit val resultToString = CanToString[TestOutcome](_.toString,
    {
      case "passed" => TestPassed
      case "failed" => TestFailed
      case "error" => TestError
      case "skipped" => TestSkipped
      case other => throw new PicklingException(s"Unrecognized TestOutcome $other")
    })

  implicit val picklerUnpickler: SPickler[TestOutcome] with Unpickler[TestOutcome] =
    canToStringPickler[TestOutcome]
}

final case class Position(sourcePath: Option[String],
  sourceFile: Option[File],
  line: Option[Int],
  lineContent: String,
  offset: Option[Int],
  pointer: Option[Int],
  pointerSpace: Option[String])

/** A compilation issue from the compiler. */
final case class CompilationFailure(
  project: ProjectReference,
  position: Position,
  severity: xsbti.Severity,
  message: String)

object CompilationFailure extends TaskEventUnapply[CompilationFailure]

/* Results related to compilation.
 * TODO let's put all of these in some kind of namespace because
 * there are a bunch of super-generic names like Analysis, API, Package, etc.
 */

// TODO what fields are truly needed in Analysis?
final case class Analysis( /* stamps: Stamps,
  apis: APIs,
  relations: Relations,
  infos: SourceInfos,
  compilations: Compilations */ )
object Analysis {
  val empty: Analysis = Analysis() /*stamps = Stamps.empty,
    apis = APIs.empty,
    relations = Relations.empty,
    infos = SourceInfos.empty,
    compilations = Compilations.empty) */

  implicit val pickler = genPickler[Analysis]
  implicit val unpickler = genUnpickler[Analysis]
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
final case class SourceInfo(reportedProblems: Vector[Problem],
  unreportedProblems: Vector[Problem])
final case class SourceInfos(allInfos: Map[File, SourceInfo])
object SourceInfos {
  val empty: SourceInfos = SourceInfos(allInfos = Map.empty[File, SourceInfo])
}
final case class Problem(category: String,
  severity: xsbti.Severity,
  message: String,
  position: Position)
object Problem {
  implicit val pickler = genPickler[Problem]
  implicit val unpickler = genUnpickler[Problem]
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
final case class ThePackage(name: String)
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
final case class TypeParameter(id: String, annotations: Vector[Annotation], typeParameters: Vector[TypeParameter], variance: xsbti.api.Variance, lowerBound: Type, upperBound: Type)
sealed trait PathComponent
final case class Id(id: String) extends PathComponent
final case class Super(qualifier: Path) extends PathComponent
final case object This extends PathComponent
final case class Path(components: Vector[PathComponent])
sealed trait Type
object Type {

}
sealed trait SimpleType extends Type
final case class Singleton(path: Path) extends SimpleType
final case class Projection(prefix: SimpleType, id: String) extends SimpleType
final case class Parameterized(baseType: SimpleType, typeArguments: Vector[Type]) extends SimpleType
final case class ParameterRef(id: String) extends SimpleType
final case object EmptyType extends SimpleType
final case class Annotated(baseType: Type, annotations: Vector[Annotation]) extends Type
final case class Structure(parents: Vector[Type], declared: Vector[Definition], inherited: Vector[Definition]) extends Type
final case class Polymorphic(baseType: Type, parameters: Vector[TypeParameter]) extends Type
final case class Existential(baseType: Type, clause: Vector[TypeParameter]) extends Type
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
  arguments: Vector[AnnotationArgument])
final case class Definition(name: String,
  access: Access,
  modifiers: Modifiers,
  annotations: Vector[Annotation])
final case class SourceAPI(packages: Vector[ThePackage],
  definitions: Vector[Definition])
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
  outputs: Vector[OutputSetting])
final case class Compilations(allCompilations: Vector[Compilation])
object Compilations {
  val empty: Compilations = Compilations(allCompilations = Vector.empty[Compilation])
}

final class CompileFailedException(message: String, cause: Throwable, val problems: Vector[Problem]) extends Exception(message, cause)

object CompileFailedException {
  import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader }
  implicit object picklerUnpickler extends SPickler[CompileFailedException] with Unpickler[CompileFailedException] {
    val tag: FastTypeTag[CompileFailedException] = implicitly[FastTypeTag[CompileFailedException]]
    private val stringOptTag = implicitly[FastTypeTag[Option[String]]]
    private val stringOptPickler = implicitly[SPickler[Option[String]]]
    private val stringOptUnpickler = implicitly[Unpickler[Option[String]]]
    private val throwableOptTag = implicitly[FastTypeTag[Option[Throwable]]]
    private val throwableOptPickler = implicitly[SPickler[Option[Throwable]]]
    private val throwableOptUnpickler = implicitly[Unpickler[Option[Throwable]]]
    private val vectorProblemTag = implicitly[FastTypeTag[Vector[Problem]]]
    private val vectorProblemPickler = implicitly[SPickler[Vector[Problem]]]
    private val vectorProblemUnpickler = implicitly[Unpickler[Vector[Problem]]]

    def pickle(a: CompileFailedException, builder: PBuilder): Unit = {
      builder.beginEntry(a)
      builder.putField("message", { b =>
        b.hintTag(stringOptTag)
        stringOptPickler.pickle(Option(a.getMessage), b)
      })
      builder.putField("cause", { b =>
        b.hintTag(throwableOptTag)
        throwableOptPickler.pickle(Option(a.getCause), b)
      })
      builder.putField("problems", { b =>
        b.hintTag(vectorProblemTag)
        vectorProblemPickler.pickle(a.problems, b)
      })
      builder.endEntry()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      val reader1 = preader.readField("message")
      reader1.hintTag(stringOptTag)
      val message = stringOptUnpickler.unpickle(stringOptTag, reader1).asInstanceOf[Option[String]]
      val reader2 = preader.readField("cause")
      reader2.hintTag(throwableOptTag)
      val cause = throwableOptUnpickler.unpickle(throwableOptTag, reader2).asInstanceOf[Option[Throwable]]
      val reader3 = preader.readField("problems")
      reader3.hintTag(vectorProblemTag)
      val problems = vectorProblemUnpickler.unpickle(vectorProblemTag, reader3).asInstanceOf[Vector[Problem]]
      new CompileFailedException(message.orNull, cause.orNull, problems)
    }
  }
}

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

  // TODO what a mess, this isn't quite right I'm sure, but probably we just
  // don't need byte arrays anyhow (we don't need all of Analysis)
  import scala.pickling.{ SPickler, Unpickler, PBuilder, PReader, FastTypeTag, PicklingException }
  implicit val picklerUnpickler: SPickler[ByteArray] with Unpickler[ByteArray] = new SPickler[ByteArray] with Unpickler[ByteArray] {
    private implicit val arrayPickler = implicitly[SPickler[Array[Byte]]]
    private implicit val arrayUnpickler = implicitly[Unpickler[Array[Byte]]]
    override val tag = implicitly[FastTypeTag[ByteArray]]

    def pickle(array: ByteArray, builder: PBuilder): Unit = {
      val concrete = array match {
        case c: ConcreteByteArray => c
      }
      arrayPickler.pickle(concrete.ary, builder)
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      arrayUnpickler.unpickle(tag, preader) match {
        case ary: Array[_] => new ConcreteByteArray(ary.asInstanceOf[Array[Byte]])
        case other => throw new PicklingException(s"expected byte array got $other")
      }
    }
  }
}

final case class ModuleId(organization: String, name: String, attributes: Map[String, String])

private[sbt] object StructurallyEqual {
  // Exclude sourceFile from comparison
  def equals(lhs: xsbti.Position, rhs: xsbti.Position): Boolean =
    equals(lhs.line, rhs.line) &&
      equals(lhs.offset, rhs.offset) &&
      equals(lhs.pointer, rhs.pointer) &&
      equals(lhs.pointerSpace, rhs.pointerSpace) &&
      equals(lhs.sourcePath, rhs.sourcePath) &&
      equals(lhs.lineContent, rhs.lineContent)

  def equals[A](lhs: xsbti.Maybe[A], rhs: xsbti.Maybe[A]): Boolean =
    if (lhs.isDefined != rhs.isDefined) false
    else if (!lhs.isDefined) true
    else (lhs.get == rhs.get)

  def equals(lhs: String, rhs: String): Boolean = lhs == rhs
}

// TODO currently due to a pickling bug caused by a Scala bug,
// the macros won't know all the subtypes of Message if we
// put this companion object earlier in the file.
object Message {

  import scala.pickling.{ SPickler, Unpickler }
  import scala.pickling.static._

  // These various picklers are mostly alphabetical except when
  // they have to be sorted in dependency order.

  // Picklers for types that appear in messages

  private implicit val backgroundJobInfoPickler = genPickler[BackgroundJobInfo]
  private implicit val backgroundJobInfoUnpickler = genUnpickler[BackgroundJobInfo]
  private implicit val clientInfoPickler = genPickler[ClientInfo]
  private implicit val clientInfoUnpickler = genUnpickler[ClientInfo]
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
  private implicit val coreLogEventPickler = genPickler[CoreLogEvent]
  private implicit val coreLogEventUnpickler = genUnpickler[CoreLogEvent]
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
  private implicit val readLineRequestPickler = genPickler[ReadLineRequest]
  private implicit val readLineRequestUnpickler = genUnpickler[ReadLineRequest]
  private implicit val readLineResponsePickler = genPickler[ReadLineResponse]
  private implicit val readLineResponseUnpickler = genUnpickler[ReadLineResponse]
  private implicit val receivedResponsePickler = genPickler[ReceivedResponse]
  private implicit val receivedResponseUnpickler = genUnpickler[ReceivedResponse]
  private implicit val registerClientRequestPickler = genPickler[RegisterClientRequest]
  private implicit val registerClientRequestUnpickler = genUnpickler[RegisterClientRequest]
  private implicit val sendSyntheticBuildChangedPickler = genPickler[SendSyntheticBuildChanged]
  private implicit val sendSyntheticBuildChangedUnpickler = genUnpickler[SendSyntheticBuildChanged]
  private implicit val sendSyntheticValueChangedPickler = genPickler[SendSyntheticValueChanged]
  private implicit val sendSyntheticValueChangedUnpickler = genUnpickler[SendSyntheticValueChanged]
  private implicit val taskEventPickler = genPickler[TaskEvent]
  private implicit val taskEventUnpickler = genUnpickler[TaskEvent]
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

  private implicit val requestPickler = genPickler[Request]
  private implicit val requestUnpickler = genUnpickler[Request]
  private implicit val responsePickler = genPickler[Response]
  private implicit val responseUnpickler = genUnpickler[Response]
  private implicit val executionEngineEventPickler = genPickler[ExecutionEngineEvent]
  private implicit val executionEngineEventUnpickler = genUnpickler[ExecutionEngineEvent]
  private implicit val eventPickler = genPickler[Event]
  private implicit val eventUnpickler = genUnpickler[Event]

  implicit val pickler: SPickler[Message] = genPickler[Message]
  implicit val unpickler: Unpickler[Message] = genUnpickler[Message]
}
