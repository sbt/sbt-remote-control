/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
package sbt.server

import java.io.File
import sbt.Path._
import sbt.IO
import org.junit.Assert._
import org.junit._
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import sbt.protocol
import sbt.protocol._
import sbt.serialization._

object ProtocolGenerators {
  import scala.annotation.tailrec

  def listOfN[T](maxDepth: Int = 1)(min: Int = 0, max: Int = 5, gen: Gen[T]): Gen[List[T]] = {
    @tailrec
    def internalListOfN(n: Int, accum: List[T]): List[T] =
      if (n == 0) accum
      else internalListOfN(n - 1, gen.sample.get :: accum)

    if (maxDepth > 0) Gen.const(internalListOfN(Gen.choose(min, max).sample.get, Nil))
    else Gen.const(Nil)
  }

  def genSet[T](max: Int = 3, gen: Gen[T]): Gen[Set[T]] =
    for {
      size <- Gen.choose(0, max)
      items <- Gen.containerOfN[Set, T](size, gen)
    } yield items

  def genMap[T, U](max: Int = 3, gen: Gen[(T, U)]): Gen[Map[T, U]] =
    for {
      size <- Gen.choose(0, max)
      items <- Gen.mapOfN[T, U](size, gen)
    } yield items

  implicit val arbitraryFile: Arbitrary[java.io.File] = Arbitrary {
    val genPathElement = for {
      chars <- listOfN()(1, 15, Gen.alphaNumChar)
    } yield chars.mkString

    val genPath = for {
      elems <- listOfN()(1, 15, genPathElement)
    } yield elems.mkString("file:/", "/", ".ext")

    for (p <- genPath) yield new java.io.File(new java.net.URI(p))
  }

  def genFileSet(max: Int = 3) = genSet[java.io.File](max, arbitraryFile.arbitrary)

  def genMaybe[T](gen: Gen[T]): Gen[xsbti.Maybe[T]] = Gen.option(gen).map {
    case Some(x) => xsbti.Maybe.just(x)
    case None => xsbti.Maybe.nothing()
  }

  private def toInteger(in: Int): Integer = new java.lang.Integer(in)

  implicit val arbitraryPosition: Arbitrary[Position] = Arbitrary {
    for {
      line <- Gen.option(arbitrary[Int])
      lineContent <- Gen.alphaStr
      offset <- Gen.option(arbitrary[Int])
      pointer <- Gen.option(arbitrary[Int])
      pointerSpace <- Gen.option(Gen.alphaStr)
      sourcePath <- Gen.option(Gen.alphaStr)
      sourceFile <- Gen.option(arbitraryFile.arbitrary)
    } yield Position(sourcePath,
      sourceFile,
      line,
      lineContent,
      offset,
      pointer,
      pointerSpace)
  }

  implicit val arbitrarySeverity: Arbitrary[xsbti.Severity] = Arbitrary(Gen.oneOf(xsbti.Severity.values.toSeq))

  implicit val arbitraryProblem: Arbitrary[Problem] = Arbitrary {
    for {
      category <- Gen.alphaStr
      severity <- arbitrarySeverity.arbitrary
      message <- Gen.alphaStr
      position <- arbitraryPosition.arbitrary
    } yield protocol.Problem(category,
      severity,
      message,
      position)
  }
}

final case class PlayStartedEvent(port: Int)
object PlayStartedEvent extends protocol.TaskEventUnapply[PlayStartedEvent] {
  implicit val pickler = genPickler[PlayStartedEvent]
  implicit val unpickler = genUnpickler[PlayStartedEvent]
}

class ProtocolTest {
  import ProtocolGenerators._

  private def addWhatWeWerePickling[T, U](t: T)(body: => U): U = try body
  catch {
    case e: Throwable =>
      e.printStackTrace()
      throw new AssertionError(s"Crash pickling ${t.getClass.getName}: ${e.getMessage}", e)
  }

  private def addWhatWeWereUnpickling[U](json: String)(body: => U): U = try body
  catch {
    case e: Throwable =>
      throw new AssertionError(s"Crash unpickling: ${e.getMessage} \n\t * json was ${json}\n", e)
  }

  @Test
  def testRawStructure(): Unit = {
    val key = protocol.AttributeKey("name", TypeExpression("java.lang.String", Nil))
    val build = new java.net.URI("file:///test/project")
    val scope = protocol.SbtScope(project = Some(
      protocol.ProjectReference(build, "test")))
    val scopedKey = protocol.ScopedKey(key, scope)
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Vector(build),
      projects = Vector(protocol.MinimalProjectStructure(scope.project.get, Vector("com.foo.Plugin"))))

    val specifics = Seq(
      // Requests
      protocol.DaemonRequest(true),
      protocol.KillServerRequest(),
      protocol.UnknownMessage(SerializedValue.fromJsonString("""{ "foo" : 42 }""")),
      protocol.ReadLineRequest(42, "HI", true),
      protocol.ReadLineResponse(Some("line")),
      protocol.ConfirmRequest(43, "msg"),
      protocol.ConfirmResponse(true),
      protocol.ReceivedResponse(),
      protocol.CommandCompletionsRequest("He", 2),
      protocol.CommandCompletionsResponse(Vector(protocol.Completion("llo", "Hello", true))),
      protocol.ListenToEvents(),
      protocol.ListenToBuildChange(),
      protocol.ExecutionRequest("test command string"),
      protocol.ListenToValue(scopedKey),
      protocol.CancelExecutionRequest(1),
      // Responses
      protocol.ErrorResponse("ZOMG"),
      protocol.CancelExecutionResponse(false),
      // Events
      // TODO - CompilationFailure
      protocol.TaskStarted(47, 1, Some(scopedKey)),
      protocol.TaskFinished(48, 1, Some(scopedKey), true, None),
      protocol.TaskStarted(47, 1, None),
      protocol.TaskFinished(48, 1, None, true, None),
      protocol.TaskStarted(49, 2, Some(scopedKey)),
      protocol.TaskFinished(50, 2, Some(scopedKey), true, None),
      protocol.BuildStructureChanged(buildStructure),
      // equals() doesn't work on Exception so we can't actually check this easily
      //protocol.ValueChanged(scopedKey, protocol.TaskFailure("O NOES", protocol.BuildValue(new Exception("Exploded"), serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue("HI"))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true))),
      // TODO make Unit work ?
      // protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(()))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f))),
      protocol.TaskLogEvent(1, protocol.LogStdOut("Hello, world")),
      protocol.DetachedLogEvent(protocol.LogStdOut("Hello, world")),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestPassed, None, 0)),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestFailed, None, 0)),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestError, None, 0)),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestSkipped, None, 0)),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestCanceled, None, 0)),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestIgnored, None, 0)),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestPending, None, 0)),
      protocol.ExecutionWaiting(41, "foo", protocol.ClientInfo(java.util.UUID.randomUUID.toString, "foo", "FOO", ProtocolVersion1, Vector(FeatureTagUnknown))),
      protocol.ExecutionStarting(56),
      protocol.ExecutionFailure(42),
      protocol.ExecutionSuccess(44),
      protocol.TaskLogEvent(2, protocol.LogMessage(protocol.LogMessage.INFO, "TEST")),
      protocol.TaskLogEvent(3, protocol.LogMessage(protocol.LogMessage.ERROR, "TEST")),
      protocol.TaskLogEvent(4, protocol.LogMessage(protocol.LogMessage.WARN, "TEST")),
      protocol.TaskLogEvent(5, protocol.LogMessage(protocol.LogMessage.DEBUG, "TEST")),
      protocol.TaskLogEvent(6, protocol.LogStdErr("TEST")),
      protocol.TaskLogEvent(7, protocol.LogStdOut("TEST2")),
      protocol.TaskEvent(8, PlayStartedEvent(port = 10)),
      protocol.BackgroundJobStarted(9, protocol.BackgroundJobInfo(id = 67, humanReadableName = "foojob", spawningTask = scopedKey)),
      protocol.BackgroundJobFinished(9, 67),
      protocol.BackgroundJobEvent(67, PlayStartedEvent(port = 10)),
      protocol.AnalyzeExecutionResponse(ExecutionAnalysisKey(Vector(scopedKey))),
      protocol.AnalyzeExecutionResponse(ExecutionAnalysisError("blah blah")),
      protocol.AnalyzeExecutionResponse(ExecutionAnalysisCommand(Some("foobar"))))

    for (s <- specifics) {
      def fromRaw(j: SerializedValue): Message =
        addWhatWeWereUnpickling(j.toJsonString)(j.parse[Message].get)
      def toRaw(m: Message): SerializedValue =
        addWhatWeWerePickling(m)(SerializedValue(m))
      val roundtrippedOption = fromRaw(toRaw(s))
      assertEquals(s"Failed to serialize:\n$s\n\n${toRaw(s)}\n\n${s.getClass}\n\n$roundtrippedOption\n\n", s, roundtrippedOption)
    }

    /*
[error] Test sbt.server.ProtocolTest.testRawStructure failed: Failed to serialize:
[error] ReadLineRequest(42,HI,true)
[error]
[error] {"$type":"sbt.protocol.ReadLineRequest","mask":true,"prompt":"HI","executionId":42.0}
[error]
[error] class sbt.protocol.ReadLineRequest
[error]
[error] None expected:<Some(ReadLineRequest(42,HI,true))> but was:<None>

     */

    protocol.TaskEvent(4, protocol.TestEvent("name", Some("foo"), protocol.TestPassed, Some(new Exception("bar")), 0)) match {
      case protocol.TestEvent(taskId, test) =>
        assertEquals(4, taskId)
        assertEquals("name", test.name)
        assertEquals(Some("foo"), test.description)
        assertEquals(Some("bar"), test.error.map(_.getMessage))
    }

    // check TaskEvent unpacking using TaskEventUnapply
    protocol.TaskEvent(8, PlayStartedEvent(port = 10)) match {
      case PlayStartedEvent(taskId, playStarted) =>
        assertEquals(8, taskId)
        assertEquals(10, playStarted.port)
      case other => throw new AssertionError("nobody expects PlayStartedEvent to be " + other)
    }
  }

  private trait Roundtripper {
    def roundtrip[T: Manifest](t: T): Unit
    def roundtripPropTest[T: Manifest](t: T): Boolean
  }

  object FakeXsbtiPosition extends xsbti.Position {
    override def line = xsbti.Maybe.just(10)
    override def offset = xsbti.Maybe.just(11)
    override def pointer = xsbti.Maybe.just(12)
    override def pointerSpace = xsbti.Maybe.just("foo")
    override def sourcePath = xsbti.Maybe.just("/temp/bar")
    override def sourceFile = xsbti.Maybe.just(new java.io.File("/temp/bar"))
    override def lineContent = "this is some stuff on the line"
    // note, this does not have a useful equals/hashCode with other instances of Position

    override def equals(o: Any): Boolean = o match {
      case pos: xsbti.Position => protocol.StructurallyEqual.equals(this, pos)
      case _ => false
    }
  }

  val FakePosition = SbtToProtocolUtils.positionToProtocol(FakeXsbtiPosition)

  private def roundtripTest(roundtripper: Roundtripper): Unit = {
    val ds = DynamicSerialization.defaultSerializations

    def roundtrip[T: Manifest](t: T): Unit = roundtripper.roundtrip(t)
    def roundtripPropTest[T: Manifest](t: T): Boolean = roundtripper.roundtripPropTest(t)

    val key = protocol.AttributeKey("name", TypeExpression("java.lang.String", Nil))
    val build = new java.net.URI("file:///test/project")
    val projectRef = protocol.ProjectReference(build, "test")
    val scope = protocol.SbtScope(project = Some(projectRef))
    val scopedKey = protocol.ScopedKey(key, scope)
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Vector(build),
      projects = Vector(protocol.MinimalProjectStructure(scope.project.get, Vector("com.foo.Plugin"))))

    roundtrip("Foo")
    roundtrip(new java.io.File("/tmp"))
    roundtrip(true)
    roundtrip(false)
    roundtrip(10: Short)
    roundtrip(11)
    roundtrip(12L)
    roundtrip(13.0f)
    roundtrip(14.0)
    roundtrip(None: Option[String])
    roundtrip(Some("Foo"))
    roundtrip(Some(true))
    roundtrip(Some(10))
    roundtrip(Nil: List[String])
    roundtrip(List("Bar", "Baz"))
    roundtrip(List(1, 2, 3))
    roundtrip(List(true, false, true, true, false))
    roundtrip(List(new java.io.File("/foo"), new java.io.File("/bar")))
    roundtrip(Vector("Bar", "Baz"))
    roundtrip(Vector(1, 2, 3))
    roundtrip(Vector(true, false, true, true, false))
    roundtrip(Vector(new java.io.File("/foo"), new java.io.File("/bar")))
    roundtrip(Seq("Bar", "Baz"))
    roundtrip(Seq(1, 2, 3))
    roundtrip(Seq(true, false, true, true, false))
    roundtrip(Seq(new java.io.File("/foo"), new java.io.File("/bar")))
    roundtrip(key)
    roundtrip(build)
    roundtrip(projectRef)
    roundtrip(scope)
    roundtrip(scopedKey)
    roundtrip(buildStructure)
    roundtrip(new Exception(null, null))
    roundtrip(new Exception("fail fail fail", new RuntimeException("some cause")))
    roundtrip(new protocol.CompileFailedException("the compile failed", null,
      Vector(protocol.Problem("something", xsbti.Severity.Error, "stuff didn't go well", FakePosition))))
    roundtrip(protocol.ModuleId(organization = "com.foo", name = "bar", attributes = Map("a" -> "b")))
  }

  private sealed trait TripDirection
  private case object ObjectToJson extends TripDirection
  private case object JsonToObject extends TripDirection
  private def oneWayTripTest(td: TripDirection, baseDir: File): Unit = {
    val ds0 = DynamicSerialization.defaultSerializations
    val ds = ds0.register(PicklerUnpickler(implicitly[Pickler[protocol.Message]],
      implicitly[Unpickler[protocol.Message]]))

    def oneWayTripBase[T: Manifest](t: T)(p: File => File)(f: (T, T) => Unit)(e: (Throwable, Throwable) => Unit): Unit = {
      val path = p(baseDir)
      val formatOption = ds.lookup(implicitly[Manifest[T]])
      formatOption map { format =>
        td match {
          case ObjectToJson =>
            val json = addWhatWeWerePickling(t)(SerializedValue(t)(format).toJsonString)
            if (path.exists) sys.error(s"$path already exists!")
            else IO.write(path, json, IO.utf8)
          case JsonToObject =>
            if (!path.exists) { sys.error(s"$path didn't exist, maybe create with: ${SerializedValue(t)(format).toJsonString}.") }
            else {
              val json = SerializedValue.fromJsonString(IO.read(path, IO.utf8))
              val parsed = addWhatWeWereUnpickling(json.toJsonString + "\n\t\t * from file: " + path)(json.parse[T](format).get)
              (t, parsed) match {
                // Throwable has a not-very-useful equals() in this case
                case (t: Throwable, parsed: Throwable) => e(t, parsed)
                case _ => f(t, parsed)
              }
            }
        }
      } getOrElse { throw new AssertionError(s"No dynamic serialization for ${t.getClass.getName}: $t") }
    }
    def oneWayTrip[T: Manifest](t: T)(p: File => File): Unit =
      oneWayTripBase[T](t)(p)((a, b) =>
        if (a == b) ()
        else sys.error(s"one-way trip of $a.\nexpected: $a: ${a.getClass.getName}\nactual: $b: ${b.getClass.getName}\nfile: ${p(baseDir)}")) { (a, b) =>
        assertEquals("one-way trip of message " + a.getMessage, a.getMessage, b.getMessage)
      }
    val key = protocol.AttributeKey("name", TypeExpression("java.lang.String", Nil))
    val build = new java.net.URI("file:///test/project")
    val projectRef = protocol.ProjectReference(build, "test")
    val scope = protocol.SbtScope(project = Some(projectRef))
    val scopedKey = protocol.ScopedKey(key, scope)
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Vector(build),
      projects = Vector(protocol.MinimalProjectStructure(scope.project.get, Vector("com.foo.Plugin"))))

    // simple data type
    oneWayTrip("Foo") { _ / "simple" / "string.json" }
    oneWayTrip(new java.io.File("/tmp")) { _ / "simple" / "file.json" }
    oneWayTrip(true) { _ / "simple" / "true.json" }
    oneWayTrip(false) { _ / "simple" / "false.json" }
    oneWayTrip(10: Short) { _ / "simple" / "short.json" }
    oneWayTrip(11) { _ / "simple" / "int.json" }
    oneWayTrip(12L) { _ / "simple" / "long.json" }
    oneWayTrip(13.0f) { _ / "simple" / "float.json" }
    oneWayTrip(14.0) { _ / "simple" / "double.json" }
    oneWayTrip(None: Option[String]) { _ / "simple" / "none.json" }
    oneWayTrip(Some("Foo")) { _ / "simple" / "some_string.json" }
    oneWayTrip(Some(true)) { _ / "simple" / "some_boolean.json" }
    oneWayTrip(Some(10)) { _ / "simple" / "some_int.json" }
    oneWayTrip(build) { _ / "simple" / "build.json" }

    // arrays
    // TODO: Support for arrays
    // oneWayTrip(Array(): Array[String]) { _ / "array" / "empty.json" }
    // oneWayTrip(Array("Bar", "Baz")) { _ / "array" / "string.json" }
    // oneWayTrip(Array(1, 2, 3)) { _ / "array" / "int.json" }
    // oneWayTrip(Array(1.0, 2, 3.0)) { _ / "array" / "double.json" }
    // oneWayTrip(Array(true, false, true, true, false)) { _ / "array" / "boolean.json" }

    // lists
    oneWayTrip(Nil: List[String]) { _ / "list" / "nil_as_list.json" }
    oneWayTrip(List("Bar", "Baz")) { _ / "list" / "string.json" }
    oneWayTrip(List(1, 2, 3)) { _ / "list" / "int.json" }
    oneWayTrip(List(1.0, 2, 3.0)) { _ / "list" / "double.json" }
    oneWayTrip(List(true, false, true, true, false)) { _ / "list" / "boolean.json" }

    // vectors
    oneWayTrip(Vector()) { _ / "vector" / "empty.json" }
    oneWayTrip(Vector("Bar", "Baz")) { _ / "vector" / "string.json" }
    oneWayTrip(Vector(1, 2, 3)) { _ / "vector" / "int.json" }
    oneWayTrip(Vector(true, false, true, true, false)) { _ / "vector" / "boolean.json" }
    oneWayTrip(Vector(1.0, 2, 3.0)) { _ / "vector" / "double.json" }

    // seqs
    oneWayTrip(Seq()) { _ / "seq" / "empty.json" }
    oneWayTrip(Seq("Bar", "Baz")) { _ / "seq" / "string.json" }
    oneWayTrip(Seq(1, 2, 3)) { _ / "seq" / "int.json" }
    oneWayTrip(Seq(true, false, true, true, false)) { _ / "seq" / "boolean.json" }
    oneWayTrip(Seq(1.0, 2, 3.0)) { _ / "seq" / "double.json" }

    // complex data type
    oneWayTrip(key) { _ / "complex" / "key.json" }
    oneWayTrip(projectRef) { _ / "complex" / "project_ref.json" }
    oneWayTrip(scope) { _ / "complex" / "scope.json" }
    oneWayTrip(scopedKey) { _ / "complex" / "scoped_key.json" }
    oneWayTrip(buildStructure) { _ / "complex" / "build_structure.json" }
    oneWayTrip(protocol.ModuleId(organization = "com.foo", name = "bar", attributes = Map("a" -> "b"))) { _ / "complex" / "moduleid.json" }
    oneWayTrip(new Exception(null, null)) { _ / "complex" / "empty_exception.json" }
    oneWayTrip(new Exception("fail fail fail", new RuntimeException("some cause"))) { _ / "complex" / "exception.json" }
    oneWayTrip(new protocol.CompileFailedException("the compile failed", null,
      Vector(protocol.Problem("something", xsbti.Severity.Error, "stuff didn't go well", FakePosition)))) { _ / "complex" / "compile_failed.json" }

    // message
    oneWayTrip[Message](protocol.RegisterClientRequest(protocol.ClientInfo("350954c2-6bf0-4925-b066-3bf20f32906b", "foo", "FOO", ProtocolVersion1, Vector(FeatureTagUnknown)))) { _ / "message" / "register_client_request.json" }
    oneWayTrip[Message](protocol.RegisterClientResponse(protocol.ServerInfo(ProtocolVersion1, Vector(FeatureTagUnknown)))) { _ / "message" / "register_client_response.json" }
    oneWayTrip[Message](protocol.DaemonRequest(true)) { _ / "message" / "daemon_req.json" }
    oneWayTrip[Message](protocol.KillServerRequest()) { _ / "message" / "kill_server_req.json" }
    oneWayTrip[Message](protocol.ReadLineRequest(42, "HI", true)) { _ / "message" / "readline_request.json" }
    oneWayTrip[Message](protocol.ReadLineResponse(Some("line"))) { _ / "message" / "readline_response.json" }
    oneWayTrip[Message](protocol.ConfirmRequest(43, "msg")) { _ / "message" / "confirm_request.json" }
    oneWayTrip[Message](protocol.ReadLineResponse(Some("line"))) { _ / "message" / "confirm_response.json" }
    oneWayTrip[Message](protocol.ReceivedResponse()) { _ / "message" / "received_response.json" }
    oneWayTrip[Message](protocol.CommandCompletionsRequest("He", 2)) { _ / "message" / "completion_request.json" }
    oneWayTrip[Message](protocol.CommandCompletionsResponse(Vector(protocol.Completion("llo", "Hello", true)))) { _ / "message" / "completion_response.json" }
    oneWayTrip[Message](protocol.ListenToEvents()) { _ / "message" / "listen_to_events.json" }
    oneWayTrip[Message](protocol.ListenToBuildChange()) { _ / "message" / "listen_to_build_change.json" }
    oneWayTrip[Message](protocol.ExecutionRequest("test command string")) { _ / "message" / "exec_request.json" }
    oneWayTrip[Message](protocol.ListenToValue(scopedKey)) { _ / "message" / "listen_to_value.json" }
    oneWayTrip[Message](protocol.CancelExecutionRequest(1)) { _ / "message" / "cancel_exec_request.json" }
    oneWayTrip[Message](protocol.ErrorResponse("ZOMG")) { _ / "message" / "error_response.json" }
    oneWayTrip[Message](protocol.CancelExecutionResponse(false)) { _ / "message" / "cancel_exec_response.json" }

    // event
    oneWayTrip[Message](protocol.TaskStarted(47, 1, Some(scopedKey))) { _ / "event" / "task_started.json" }
    oneWayTrip[Message](protocol.TaskFinished(48, 1, Some(scopedKey), true, None)) { _ / "event" / "task_finished.json" }
    oneWayTrip[Message](protocol.TaskStarted(47, 1, None)) { _ / "event" / "task_started_none.json" }
    oneWayTrip[Message](protocol.TaskFinished(48, 1, None, true, None)) { _ / "event" / "task_finished_none.json" }
    oneWayTrip[Message](protocol.TaskFinished(48, 1, Some(scopedKey), false, Some("error message here"))) { _ / "event" / "task_finished_failed.json" }
    oneWayTrip[Message](protocol.BuildStructureChanged(buildStructure)) { _ / "event" / "build_structure_changed.json" }
    // oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskFailure(protocol.BuildValue(new Exception("Exploded"), serializations)))) {
    //   _ / "event" / "value_changed_task_failure.json"
    // }
    oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue("HI")))) { _ / "event" / "value_changed" / "string.json" }
    oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42)))) { _ / "event" / "value_changed" / "int.json" }
    oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L)))) { _ / "event" / "value_changed" / "long.json" }
    oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true)))) { _ / "event" / "value_changed" / "boolean.json" }
    // oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(())))) { _ / "event" / "value_changed" / "unit.json" }
    oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0)))) { _ / "event" / "value_changed" / "double.json" }
    oneWayTrip[Message](protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f)))) { _ / "event" / "value_changed" / "float.json" }
    oneWayTrip[Message](protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestPassed, None, 0))) { _ / "event" / "task_event.json" }
    oneWayTrip[Message](protocol.ExecutionWaiting(41, "foo", protocol.ClientInfo("350954c2-6bf0-4925-b066-3bf20f32906b", "foo", "FOO", ProtocolVersion1, Vector(FeatureTagUnknown)))) { _ / "event" / "exec_waiting.json" }
    oneWayTrip[Message](protocol.ExecutionStarting(56)) { _ / "event" / "exec_starting.json" }
    oneWayTrip[Message](protocol.ExecutionFailure(42)) { _ / "event" / "exec_failure.json" }
    oneWayTrip[Message](protocol.ExecutionSuccess(44)) { _ / "event" / "exec_success.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(1, protocol.LogStdOut("Hello, world"))) { _ / "event" / "log" / "task_log_event.json" }
    oneWayTrip[Message](protocol.DetachedLogEvent(protocol.LogStdOut("Hello, world"))) { _ / "event" / "log" / "detached_log_event.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(2, protocol.LogMessage(protocol.LogMessage.INFO, "TEST"))) { _ / "event" / "log" / "info.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(3, protocol.LogMessage(protocol.LogMessage.ERROR, "TEST"))) { _ / "event" / "log" / "error.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(4, protocol.LogMessage(protocol.LogMessage.WARN, "TEST"))) { _ / "event" / "log" / "warn.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(5, protocol.LogMessage(protocol.LogMessage.DEBUG, "TEST"))) { _ / "event" / "log" / "debug.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(6, protocol.LogStdErr("TEST"))) { _ / "event" / "log" / "log_std_err.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(7, protocol.LogStdOut("TEST2"))) { _ / "event" / "log" / "log_std_out.json" }
    oneWayTrip[Message](protocol.TaskEvent(8, PlayStartedEvent(port = 10))) { _ / "event" / "play_started.json" }
    oneWayTrip[Message](protocol.BackgroundJobStarted(9, protocol.BackgroundJobInfo(id = 67, humanReadableName = "foojob", spawningTask = scopedKey))) { _ / "event" / "bg_started.json" }
    oneWayTrip[Message](protocol.BackgroundJobFinished(9, 67)) { _ / "event" / "bg_finished.json" }
    oneWayTrip[Message](protocol.BackgroundJobEvent(67, PlayStartedEvent(port = 10))) { _ / "event" / "bg.json" }
  }

  @Test
  def testSerializationStability(): Unit = {
    val baseDir = (new File("protocol-test")) / "src" / "test" / "resource" / "saved-protocol"

    oneWayTripTest(JsonToObject, baseDir / protocol.ProtocolVersion1.toString)

    // uncomment this line to write new files
    // oneWayTripTest(ObjectToJson, baseDir / "2")
    // oneWayTripTest(JsonToObject, baseDir / "2")
  }

  @Test
  def testDynamicSerialization(): Unit = {
    val ds = DynamicSerialization.defaultSerializations
    def roundtripBase[U, T: Manifest](t: T)(f: (T, T) => U)(e: (Throwable, Throwable) => U): U = {
      val formatOption = ds.lookup(implicitly[Manifest[T]])
      formatOption map { format =>
        val json = addWhatWeWerePickling(t)(SerializedValue(t)(format))
        //System.err.println(s"${t} = ${Json.prettyPrint(json)}")
        val parsed = addWhatWeWereUnpickling(json.toJsonString)(json.parse[T](format).get)
        (t, parsed) match {
          // Throwable has a not-very-useful equals() in this case
          case (t: Throwable, parsed: Throwable) => e(t, parsed)
          case _ => f(t, parsed)
        }
      } getOrElse { throw new AssertionError(s"No dynamic serialization for ${t.getClass.getName}: $t") }
    }

    roundtripTest(new Roundtripper {
      override def roundtrip[T: Manifest](t: T): Unit =
        roundtripBase[Unit, T](t)((a, b) => assertEquals("round trip of " + a, a, b)) { (a, b) =>
          assertEquals("round trip of message " + a.getMessage, a.getMessage, b.getMessage)
        }

      override def roundtripPropTest[T: Manifest](t: T): Boolean =
        roundtripBase[Boolean, T](t) { (a, b) =>
          val r = a == b
          if (!r) println(s"values are not equal:\n--- A: $a\n--- B: $b")
          r
        } { (a, b) =>
          val r = a.getMessage == b.getMessage
          if (!r) println(s"values are not equal:\n--- A: $a\n--- B: $b")
          r
        }
    })
  }

  @Test
  def testBuildValueSerialization(): Unit = {
    val serializations = DynamicSerialization.defaultSerializations
    def roundtripBuild[U, T: Manifest](t: T)(f: (protocol.BuildValue, protocol.BuildValue) => U)(e: (Throwable, Throwable) => U): U = {
      val mf = implicitly[Manifest[T]]
      implicit val format = serializations.lookup(mf).getOrElse(throw new AssertionError(s"no format for ${t.getClass.getName} $t"))
      val buildValue = serializations.buildValue(t)
      val json = SerializedValue(buildValue)
      // System.err.println(s"${buildValue} = ${json.toString}")
      val parsedValue = json.parse[protocol.BuildValue].get
      import scala.util.{ Success, Failure }
      val parsedT =
        parsedValue.value[T](format) match {
          case Success(v) => v
          case Failure(t) =>
            t.printStackTrace()
            throw new AssertionError(s"could not read back from build value ${t.getClass.getName} $t\n orig: $buildValue\njson: $json\nparsed: $parsedValue", t)
        }
      val buildValueClass: Class[_] = t.getClass
      // Throwable has a not-very-useful equals() in this case
      if (classOf[Throwable].isAssignableFrom(buildValueClass)) {
        val p = parsedValue.value[T](format).map(_.asInstanceOf[Throwable]).get
        e(t.asInstanceOf[Throwable], p)
      } else {
        f(buildValue, parsedValue)
      }
    }

    def roundtripBuildValue[T: Manifest](t: T): Unit =
      roundtripBuild[Unit, T](t)((x, y) => assertEquals("round trip of Serialized( " + implicitly[Manifest[T]] + ") [" + t + "]", x, y))((t, p) =>
        assertEquals("round trip of message " + t.getMessage, t.getMessage, p.getMessage))

    def roundtripBuildValueTest[T: Manifest](t: T): Boolean =
      roundtripBuild[Boolean, T](t) { (a, b) =>
        val r = a == b
        if (!r) println(s"values are not equal:\n--- A: $a\n--- B: $b")
        r
      } { (a, b) =>
        val r = a.getMessage == b.getMessage
        if (!r) println(s"values are not equal:\n--- A: $a\n--- B: $b")
        r
      }

    roundtripTest(new Roundtripper {
      override def roundtrip[T: Manifest](t: T): Unit = {
        roundtripBuildValue(t)
      }
      override def roundtripPropTest[T: Manifest](t: T): Boolean = {
        roundtripBuildValueTest(t)
      }
    })
  }
}
