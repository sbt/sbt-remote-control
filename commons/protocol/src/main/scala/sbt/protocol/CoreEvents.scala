package sbt.protocol

import java.io.File
import scala.collection.immutable
import sbt.serialization._

///// These are types that conceptually go with sbt's core and jvm plugins
///// (they would go with sbt Defaults.scala).

final case class TestGroupStarted(name: String)
object TestGroupStarted extends TaskEventUnapply[TestGroupStarted] {
  implicit val pickler: Pickler[TestGroupStarted] = genPickler[TestGroupStarted]
  implicit val unpickler: Unpickler[TestGroupStarted] = genUnpickler[TestGroupStarted]
}
final case class TestGroupFinished(name: String, result: TestGroupResult, error: Option[Throwable]) {
  // TODO add hashCode also. the point of this is to ignore the stack trace
  // in the Throwable.
  override def equals(other: Any): Boolean = other match {
    case null => false
    case that: TestGroupFinished => (this.name == that.name) &&
      (this.result == that.result) &&
      (this.error.map(_.getMessage) == that.error.map(_.getMessage))
    case _ => false
  }
}
object TestGroupFinished extends TaskEventUnapply[TestGroupFinished] {
  implicit val pickler: Pickler[TestGroupFinished] = genPickler[TestGroupFinished]
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
  import scala.pickling.PicklingException

  private implicit val resultToString = CanToString[TestGroupResult](_.toString,
    {
      case "passed" => TestGroupPassed
      case "failed" => TestGroupFailed
      case "error" => TestGroupError
      case other => throw new PicklingException(s"Unrecognized TestGroupResult $other")
    })

  implicit val picklerUnpickler: Pickler[TestGroupResult] with Unpickler[TestGroupResult] =
    canToStringPickler[TestGroupResult]
}

/** A build test has done something useful and we're being notified of it. */
final case class TestEvent(name: String, description: Option[String], outcome: TestOutcome, error: Option[Throwable], duration: Long) {
  // TODO - custom hashCode.
  // Custom equals to ignore duration and stack trace
  override def equals(other: Any): Boolean =
    other match {
      case null => false
      case that: TestEvent => (name == that.name) && (description == that.description) && (outcome == that.outcome) && (error.map(_.getMessage) == that.error.map(_.getMessage))
      case _ => false
    }
}

object TestEvent extends TaskEventUnapply[TestEvent] {
  implicit val pickler: Pickler[TestEvent] = genPickler[TestEvent]
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
case object TestCanceled extends TestOutcome {
  override def toString = "canceled"
}
case object TestIgnored extends TestOutcome {
  override def toString = "ignored"
}
case object TestPending extends TestOutcome {
  override def toString = "pending"
}

object TestOutcome {
  import scala.pickling.PicklingException
  import sbt.serialization.CanToString

  private implicit val resultToString = CanToString[TestOutcome](_.toString,
    {
      case "passed" => TestPassed
      case "failed" => TestFailed
      case "error" => TestError
      case "skipped" => TestSkipped
      case "canceled" => TestCanceled
      case "ignored" => TestIgnored
      case "pending" => TestPending
      case other => throw new PicklingException(s"Unrecognized TestOutcome $other")
    })

  implicit val picklerUnpickler: Pickler[TestOutcome] with Unpickler[TestOutcome] =
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

final case class Problem(category: String,
  severity: xsbti.Severity,
  message: String,
  position: Position)
object Problem {
  implicit val pickler = genPickler[Problem]
  implicit val unpickler = genUnpickler[Problem]
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
  // TODO - Alias these in serialziation package.
  import scala.pickling.{ FastTypeTag, PBuilder, PReader }
  implicit object picklerUnpickler extends Pickler[CompileFailedException] with Unpickler[CompileFailedException] {
    val tag: FastTypeTag[CompileFailedException] = implicitly[FastTypeTag[CompileFailedException]]
    private val stringOptTag = implicitly[FastTypeTag[Option[String]]]
    private val stringOptPickler = implicitly[Pickler[Option[String]]]
    private val stringOptUnpickler = implicitly[Unpickler[Option[String]]]
    private val throwableOptTag = implicitly[FastTypeTag[Option[Throwable]]]
    private val throwableOptPickler = implicitly[Pickler[Option[Throwable]]]
    private val throwableOptUnpickler = implicitly[Unpickler[Option[Throwable]]]
    private val vectorProblemTag = implicitly[FastTypeTag[Vector[Problem]]]
    private val vectorProblemPickler = implicitly[Pickler[Vector[Problem]]]
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
    def unpickle(tag: String, preader: PReader): Any = {
      // TODO - hint statically elided types...
      preader.hintStaticallyElidedType()
      val message = stringOptUnpickler.unpickleEntry(preader.readField("message")).asInstanceOf[Option[String]]
      val cause = throwableOptUnpickler.unpickleEntry(preader.readField("cause")).asInstanceOf[Option[Throwable]]
      val problems = vectorProblemUnpickler.unpickleEntry(preader.readField("problems")).asInstanceOf[Vector[Problem]]
      new CompileFailedException(message.orNull, cause.orNull, problems)
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
