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
import scala.pickling.internal.AppliedType

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

  implicit val arbitraryByteArray: Arbitrary[protocol.ByteArray] = Arbitrary {
    for (v <- Arbitrary.arbContainer[Array, Byte].arbitrary) yield protocol.ByteArray(v)
  }

  implicit val arbitraryStamp: Arbitrary[protocol.Stamp] = Arbitrary {
    val genHash = for (v <- arbitrary[protocol.ByteArray]) yield protocol.Hash(v)

    val genLastModified = for (v <- arbitrary[Long]) yield protocol.LastModified(v)

    val genExists = for (v <- arbitrary[Boolean]) yield protocol.Exists(v)

    Gen.oneOf(genHash, genLastModified, genExists)
  }

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

  implicit val arbitraryStamps: Arbitrary[protocol.Stamps] = Arbitrary {
    for {
      allInternalSources <- genFileSet(3)
      allBinaries <- genFileSet(3)
      allProducts <- genFileSet(3)
      sources <- genMap[java.io.File, protocol.Stamp](3, Gen.zip(arbitraryFile.arbitrary, arbitraryStamp.arbitrary))
      binaries <- genMap[java.io.File, protocol.Stamp](3, Gen.zip(arbitraryFile.arbitrary, arbitraryStamp.arbitrary))
      products <- genMap[java.io.File, protocol.Stamp](3, Gen.zip(arbitraryFile.arbitrary, arbitraryStamp.arbitrary))
      classNames <- genMap[java.io.File, String](3, Gen.zip(arbitraryFile.arbitrary, Gen.alphaStr))
    } yield protocol.Stamps(allInternalSources,
      allBinaries,
      allProducts,
      sources,
      binaries,
      products,
      classNames)
  }

  implicit def arbitraryRelation[A, B](implicit aA: Arbitrary[A], aB: Arbitrary[B]): Arbitrary[protocol.Relation[A, B]] = Arbitrary {
    for {
      forwardMap <- genMap[A, Set[B]](3, Gen.zip(aA.arbitrary, genSet[B](3, aB.arbitrary)))
      reverseMap <- genMap[B, Set[A]](3, Gen.zip(aB.arbitrary, genSet[A](3, aA.arbitrary)))
    } yield protocol.Relation[A, B](forwardMap, reverseMap)
  }

  implicit val arbitraryRelationSource: Arbitrary[protocol.RelationsSource] = Arbitrary {
    for {
      internal <- arbitraryRelation[java.io.File, java.io.File].arbitrary
      external <- arbitraryRelation[java.io.File, String].arbitrary
    } yield protocol.RelationsSource(internal, external)
  }

  implicit val arbitraryRelations: Arbitrary[protocol.Relations] = Arbitrary {
    for {
      allSources <- genFileSet()
      allProducts <- genFileSet()
      allBinaryDeps <- genFileSet()
      allInternalSrcDeps <- genFileSet()
      allExternalDeps <- genSet[String](3, Gen.alphaStr)
      srcProd <- arbitraryRelation[java.io.File, java.io.File].arbitrary
      binaryDep <- arbitraryRelation[java.io.File, java.io.File].arbitrary
      internalSrcDep <- arbitraryRelation[java.io.File, java.io.File].arbitrary
      externalDep <- arbitraryRelation[java.io.File, String].arbitrary
      direct <- Gen.option(arbitraryRelationSource.arbitrary)
      publicInherited <- Gen.option(arbitraryRelationSource.arbitrary)
      classes <- arbitraryRelation[java.io.File, String].arbitrary
    } yield protocol.Relations(allSources,
      allProducts,
      allBinaryDeps,
      allInternalSrcDeps,
      allExternalDeps,
      srcProd,
      binaryDep,
      internalSrcDep,
      externalDep,
      direct,
      publicInherited,
      classes)
  }

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

  implicit val arbitrarySourceInfo: Arbitrary[protocol.SourceInfo] = Arbitrary {
    for {
      reportedProblems <- listOfN()(0, 5, arbitraryProblem.arbitrary)
      unreportedProblems <- listOfN()(0, 5, arbitraryProblem.arbitrary)
    } yield protocol.SourceInfo(reportedProblems.toVector, unreportedProblems.toVector)
  }

  implicit val arbitrarySourceInfos: Arbitrary[protocol.SourceInfos] =
    Arbitrary(genMap(3, Gen.zip(arbitraryFile.arbitrary, arbitrarySourceInfo.arbitrary)).map(protocol.SourceInfos.apply))

  implicit val arbitraryOutputSetting: Arbitrary[protocol.OutputSetting] =
    Arbitrary(Gen.zip(Gen.alphaStr, Gen.alphaStr).map { case (a, b) => protocol.OutputSetting(a, b) })

  implicit val arbitraryCompilation: Arbitrary[protocol.Compilation] = Arbitrary {
    for {
      time <- arbitrary[Long]
      settings <- listOfN()(0, 10, arbitraryOutputSetting.arbitrary)
    } yield protocol.Compilation(time, settings.toVector)
  }

  implicit val arbitraryCompilations: Arbitrary[protocol.Compilations] = Arbitrary {
    for {
      compilations <- listOfN()(0, 10, arbitraryCompilation.arbitrary)
    } yield protocol.Compilations(compilations.toVector)
  }

  // FIXME what fields in Analysis do we need
  def genAnalysis(maxDepth: Int = 3): Gen[protocol.Analysis] =
    protocol.Analysis()
  /*
    for {
      stamps <- arbitraryStamps.arbitrary
      apis <- genAPIs(maxDepth - 1)
      relations <- arbitraryRelations.arbitrary
      infos <- arbitrarySourceInfos.arbitrary
      compilations <- arbitraryCompilations.arbitrary
    } yield protocol.Analysis(stamps,
      apis,
      relations,
      infos,
      compilations)
*/

  def genAPIs(maxDepth: Int = 3): Gen[protocol.APIs] =
    for {
      allExternals <- genSet[String](3, Gen.alphaStr)
      allInternalSources <- genFileSet(3)
      internal <- genMap[java.io.File, protocol.Source](3, Gen.zip(arbitraryFile.arbitrary, genSource(maxDepth - 1)))
      external <- genMap[String, protocol.Source](3, Gen.zip(Gen.alphaStr, genSource(maxDepth - 1)))
    } yield protocol.APIs(allExternals, allInternalSources, internal, external)

  def genSourceAPI(maxDepth: Int = 3): Gen[protocol.SourceAPI] =
    for {
      packages <- listOfN(maxDepth - 1)(0, 3, arbitraryPackage.arbitrary)
      definitions <- listOfN(maxDepth - 1)(0, 3, genDefinition(maxDepth - 1))
    } yield protocol.SourceAPI(packages.toVector, definitions.toVector)

  def genSource(maxDepth: Int = 3): Gen[protocol.Source] =
    for {
      compilation <- arbitraryCompilation.arbitrary
      hash <- arbitraryByteArray.arbitrary
      api <- genSourceAPI(maxDepth - 1)
      apiHash <- arbitrary[Int]
      hasMacro <- arbitrary[Boolean]
    } yield protocol.Source(compilation, hash, api, apiHash, hasMacro)

  implicit val arbitraryModifiers: Arbitrary[protocol.Modifiers] = Arbitrary {
    for {
      isAbstract <- arbitrary[Boolean]
      isOverride <- arbitrary[Boolean]
      isFinal <- arbitrary[Boolean]
      isSealed <- arbitrary[Boolean]
      isImplicit <- arbitrary[Boolean]
      isLazy <- arbitrary[Boolean]
      isMacro <- arbitrary[Boolean]
    } yield protocol.Modifiers(isAbstract,
      isOverride,
      isFinal,
      isSealed,
      isImplicit,
      isLazy,
      isMacro)
  }

  def genPathComponent(maxDepth: Int = 3): Gen[protocol.PathComponent] = {
    val genId = Gen.alphaStr.map(protocol.Id.apply)
    lazy val genSuper = genPath(maxDepth - 1).map(protocol.Super.apply)

    if (maxDepth > 0) Gen.oneOf(genId, genSuper, Gen.const(protocol.This))
    else Gen.const(protocol.This)
  }

  def genPath(maxDepth: Int = 3): Gen[protocol.Path] =
    for {
      path <- listOfN(maxDepth - 1)(0, 3, genPathComponent(maxDepth - 1))
    } yield protocol.Path(path.toVector)

  implicit val arbitraryPackage: Arbitrary[protocol.ThePackage] =
    Arbitrary(Gen.alphaStr.map(protocol.ThePackage.apply))

  implicit val arbitraryQualifier: Arbitrary[protocol.Qualifier] = Arbitrary {
    val genIdQualifier = Gen.alphaStr.map(protocol.IdQualifier.apply)

    Gen.oneOf(genIdQualifier, Gen.const(protocol.ThisQualifier), Gen.const(protocol.Unqualified))
  }

  implicit val arbitraryAccess: Arbitrary[protocol.Access] = Arbitrary {
    val genProtected = arbitraryQualifier.arbitrary.map(protocol.Protected.apply)
    val genPrivate = arbitraryQualifier.arbitrary.map(protocol.Private.apply)

    Gen.oneOf(genProtected, genPrivate, Gen.const(protocol.Public))
  }

  def genDefinition(maxDepth: Int = 3): Gen[protocol.Definition] =
    for {
      name <- Gen.alphaStr
      access <- arbitraryAccess.arbitrary
      modifiers <- arbitraryModifiers.arbitrary
      annotations <- listOfN(maxDepth - 1)(0, 3, genAnnotation(maxDepth - 1))
    } yield protocol.Definition(name,
      access,
      modifiers,
      annotations.toVector)

  implicit val arbitraryVariance: Arbitrary[xsbti.api.Variance] = Arbitrary(Gen.oneOf(xsbti.api.Variance.values.toSeq))

  def genTypeParameter(maxDepth: Int = 3): Gen[protocol.TypeParameter] =
    for {
      id <- Gen.alphaStr
      annotations <- listOfN(maxDepth - 1)(0, 5, genAnnotation(maxDepth - 1))
      typeParameters <- listOfN(maxDepth - 1)(0, 5, genTypeParameter(maxDepth - 1))
      variance <- arbitraryVariance.arbitrary
      lowerBound <- genType(maxDepth - 1)
      upperBound <- genType(maxDepth - 1)
    } yield protocol.TypeParameter(id,
      annotations.toVector,
      typeParameters.toVector,
      variance,
      lowerBound,
      upperBound)

  def genSimpleType(maxDepth: Int = 3): Gen[protocol.SimpleType] = {
    // sealed trait SimpleType extends Type
    val genSingleton = genPath(maxDepth - 1).map(protocol.Singleton.apply)
    lazy val genProjection =
      Gen.zip(genSimpleType(maxDepth - 1), Gen.alphaStr).map { case (a, b) => protocol.Projection(a, b) }
    lazy val genParameterized = for {
      st <- genSimpleType(maxDepth - 1)
      args <- listOfN(maxDepth - 1)(0, 2, genType(maxDepth - 1))
    } yield protocol.Parameterized(st, args.toVector)
    val genParameterRef = Gen.alphaStr.map(protocol.ParameterRef.apply)

    if (maxDepth > 0) Gen.oneOf(Gen.const(protocol.EmptyType), genSingleton, genParameterRef, genProjection, genParameterized)
    else Gen.const(protocol.EmptyType)
  }

  def genType(maxDepth: Int = 3): Gen[protocol.Type] = {
    lazy val genAnnotated = for {
      typ <- genType(maxDepth - 1)
      annotations <- listOfN(maxDepth - 1)(0, 2, genAnnotation(maxDepth - 1))
    } yield protocol.Annotated(typ, annotations.toVector)
    lazy val genStructure = for {
      parents <- listOfN(maxDepth - 1)(0, 2, genType(maxDepth - 1))
      declared <- listOfN(maxDepth - 1)(0, 2, genDefinition(maxDepth - 1))
      inherited <- listOfN(maxDepth - 1)(0, 2, genDefinition(maxDepth - 1))
    } yield protocol.Structure(parents.toVector, declared.toVector, inherited.toVector)
    lazy val genPolymorphic = for {
      typ <- genType(maxDepth - 1)
      typeParameters <- listOfN(maxDepth - 1)(0, 2, genTypeParameter(maxDepth - 1))
    } yield protocol.Polymorphic(typ, typeParameters.toVector)
    lazy val genExistential = for {
      typ <- genType(maxDepth - 1)
      typeParameters <- listOfN(maxDepth - 1)(0, 2, genTypeParameter(maxDepth - 1))
    } yield protocol.Existential(typ, typeParameters.toVector)
    lazy val genConstant = for {
      typ <- genType(maxDepth - 1)
      value <- Gen.alphaStr
    } yield protocol.Constant(typ, value)

    lazy val genExtendedTypes = Gen.oneOf(genAnnotated, genStructure, genPolymorphic, genExistential, genConstant)

    if (maxDepth > 0) Gen.oneOf(genSimpleType(maxDepth - 1), genExtendedTypes)
    else Gen.const(protocol.EmptyType)
  }

  implicit val arbitraryAnnotationArgument: Arbitrary[protocol.AnnotationArgument] =
    Arbitrary(Gen.zip(Gen.alphaStr, Gen.alphaStr).map { case (a, b) => protocol.AnnotationArgument(a, b) })

  def genAnnotation(maxDepth: Int = 3): Gen[protocol.Annotation] =
    for {
      typ <- genType(maxDepth - 1)
      args <- listOfN(maxDepth - 1)(0, 5, arbitraryAnnotationArgument.arbitrary)
    } yield protocol.Annotation(typ, args.toVector)

}

final case class PlayStartedEvent(port: Int)
object PlayStartedEvent extends protocol.TaskEventUnapply[PlayStartedEvent] {
  import scala.pickling.{ SPickler, Unpickler }
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
    val key = protocol.AttributeKey("name", AppliedType("java.lang.String", Nil))
    val build = new java.net.URI("file:///test/project")
    val scope = protocol.SbtScope(project = Some(
      protocol.ProjectReference(build, "test")))
    val scopedKey = protocol.ScopedKey(key, scope)
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Vector(build),
      projects = Vector(protocol.MinimalProjectStructure(scope.project.get, Vector("com.foo.Plugin"))))

    val specifics = Seq(
      // Requests
      protocol.KillServerRequest(),
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
      protocol.CoreLogEvent(protocol.LogStdOut("Hello, world")),
      protocol.TaskEvent(4, protocol.TestEvent("name", None, protocol.TestPassed, None, 0)),
      protocol.ExecutionWaiting(41, "foo", protocol.ClientInfo(java.util.UUID.randomUUID.toString, "foo", "FOO")),
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
      def fromRaw(j: JsonValue): Message =
        addWhatWeWereUnpickling(j.renderCompact)(j.parse[Message].get)
      def toRaw(m: Message): JsonValue =
        addWhatWeWerePickling(m)(JsonValue(m))
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

    /* //TODO commented out because it crashes the compiler
    protocol.TaskEvent(4, protocol.TestEvent("name", Some("foo"), protocol.TestPassed, Some("bar"), 0)) match {
      case protocol.TestEvent(taskId, test) =>
        assertEquals(4, taskId)
        assertEquals("name", test.name)
        assertEquals(Some("foo"), test.description)
        assertEquals(Some("bar"), test.error)
    }
*/
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

    val key = protocol.AttributeKey("name", AppliedType("java.lang.String", Nil))
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
    /* Turned off for now..
    object NonTrivialProtocol extends Properties("NonTrivialProtocol") {
      implicit val arbitraryAnalysis: Arbitrary[protocol.Analysis] = Arbitrary(genAnalysis())
      implicit val arbitraryAPIs: Arbitrary[protocol.APIs] = Arbitrary(genAPIs())
      implicit val arbitraryTypeParameter: Arbitrary[protocol.TypeParameter] = Arbitrary(genTypeParameter())
      implicit val arbitraryAnnotation: Arbitrary[protocol.Annotation] = Arbitrary(genAnnotation())
      implicit val arbitraryDefinition: Arbitrary[protocol.Definition] = Arbitrary(genDefinition())
      implicit val arbitrarySourceAPI: Arbitrary[protocol.SourceAPI] = Arbitrary(genSourceAPI())
      implicit val arbitrarySource: Arbitrary[protocol.Source] = Arbitrary(genSource())
      implicit val arbitraryType: Arbitrary[protocol.Type] = Arbitrary(genType())
      implicit val arbitrarySimpleType: Arbitrary[protocol.SimpleType] = Arbitrary(genSimpleType())
      implicit val arbitraryPath: Arbitrary[protocol.Path] = Arbitrary(genPath())

      property("protocol.Stamps") = forAll { x: protocol.Stamps => roundtripPropTest(x) }
      property("protocol.SourceInfo") = forAll { x: protocol.SourceInfo => roundtripPropTest(x) }
      property("protocol.SourceInfos") = forAll { x: protocol.SourceInfos => roundtripPropTest(x) }
      property("Problem") = forAll { x: Problem => roundtripPropTest(x) }
      property("protocol.APIs") = forAll { x: protocol.APIs => roundtripPropTest(x) }
      property("protocol.ThePackage") = forAll { x: protocol.ThePackage => roundtripPropTest(x) }
      property("protocol.TypeParameter") = forAll { x: protocol.TypeParameter => roundtripPropTest(x) }
      property("protocol.Path") = forAll { x: protocol.Path => roundtripPropTest(x) }
      property("protocol.Modifiers") = forAll { x: protocol.Modifiers => roundtripPropTest(x) }
      property("protocol.AnnotationArgument") = forAll { x: protocol.AnnotationArgument => roundtripPropTest(x) }
      property("protocol.Annotation") = forAll { x: protocol.Annotation => roundtripPropTest(x) }
      property("protocol.Definition") = forAll { x: protocol.Definition => roundtripPropTest(x) }
      property("protocol.SourceAPI") = forAll { x: protocol.SourceAPI => roundtripPropTest(x) }
      property("protocol.Source") = forAll { x: protocol.Source => roundtripPropTest(x) }
      property("protocol.RelationsSource") = forAll { x: protocol.RelationsSource => roundtripPropTest(x) }
      property("protocol.Relations") = forAll { x: protocol.Relations => roundtripPropTest(x) }
      property("protocol.OutputSetting") = forAll { x: protocol.OutputSetting => roundtripPropTest(x) }
      property("protocol.Compilation") = forAll { x: protocol.Compilation => roundtripPropTest(x) }
      property("protocol.Compilations") = forAll { x: protocol.Compilations => roundtripPropTest(x) }
      property("protocol.Stamp") = forAll { x: protocol.Stamp => roundtripPropTest(x) }
      property("protocol.Access") = forAll { x: protocol.Access => roundtripPropTest(x) }
      property("protocol.Type") = forAll { x: protocol.Type => roundtripPropTest(x) }
      property("protocol.SimpleType") = forAll { x: protocol.SimpleType => roundtripPropTest(x) }
      property("protocol.ByteArray") = forAll { x: protocol.ByteArray => roundtripPropTest(x) }
      property("protocol.Analysis") = forAll { x: protocol.Analysis => roundtripPropTest(x) }
    }
    NonTrivialProtocol.check
    roundtrip(protocol.Stamps.empty)
    roundtrip(protocol.APIs.empty)
    roundtrip(protocol.Relations.empty)
    roundtrip(protocol.SourceInfos.empty)
    roundtrip(protocol.Analysis.empty)*/
  }

  private sealed trait TripDirection
  private case object ObjectToJson extends TripDirection
  private case object JsonToObject extends TripDirection
  private def oneWayTripTest(td: TripDirection, baseDir: File): Unit = {
    val ds0 = DynamicSerialization.defaultSerializations
    val ds = ds0.register(implicitly[SbtSerializer[protocol.Message]])

    def oneWayTripBase[T: Manifest](t: T)(p: File => File)(f: (T, T) => Unit)(e: (Throwable, Throwable) => Unit): Unit = {
      val path = p(baseDir)
      val formatOption = ds.lookup(implicitly[Manifest[T]])
      formatOption map { format =>
        td match {
          case ObjectToJson =>
            val json = addWhatWeWerePickling(t)(SerializedValue(t)(format.pickler))
            // IO.write(path, json.toString, IO.utf8)
            if (path.exists) () // Just ignore things we write twice, reading into different types should work.
            else IO.write(path, json.toString, IO.utf8)
          case JsonToObject =>
            if (!path.exists) { sys.error(s"$path didn't exist, maybe create with: ${SerializedValue(t)(format.pickler)}.") }
            else {
              val json = JsonValue.parseJson(IO.read(path, IO.utf8)).get
              val parsed = addWhatWeWereUnpickling(json.renderCompact + "\n\t\t * from file: " + path)(json.parse[T](format.unpickler).get)
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
    val key = protocol.AttributeKey("name", AppliedType("java.lang.String", Nil))
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
    oneWayTrip(Nil: List[String]) { _ / "array" / "nil.json" }
    oneWayTrip(List("Bar", "Baz")) { _ / "array" / "seq_string.json" }
    oneWayTrip(List(1, 2, 3)) { _ / "array" / "seq_int.json" }
    oneWayTrip(List(1.0, 2, 3.0)) { _ / "array" / "seq_double.json" }
    oneWayTrip(List(true, false, true, true, false)) { _ / "array" / "seq_boolean.json" }
    oneWayTrip(Vector("Bar", "Baz")) { _ / "array" / "seq_string.json" }
    oneWayTrip(Vector(1, 2, 3)) { _ / "array" / "seq_int.json" }
    oneWayTrip(Vector(true, false, true, true, false)) { _ / "array" / "seq_boolean.json" }
    oneWayTrip(Vector(1.0, 2, 3.0)) { _ / "array" / "seq_double.json" }
    oneWayTrip(Seq("Bar", "Baz")) { _ / "array" / "seq_string.json" }
    oneWayTrip(Seq(1, 2, 3)) { _ / "array" / "seq_int.json" }
    oneWayTrip(Seq(true, false, true, true, false)) { _ / "array" / "seq_boolean.json" }
    // complex data type
    oneWayTrip(key) { _ / "complex" / "key.json" }
    oneWayTrip(projectRef) { _ / "complex" / "project_ref.json" }
    oneWayTrip(scope) { _ / "complex" / "scope.json" }
    oneWayTrip(scopedKey) { _ / "complex" / "scoped_key.json" }
    oneWayTrip(buildStructure) { _ / "complex" / "build_structure.json" }
    oneWayTrip(protocol.ModuleId(organization = "com.foo", name = "bar", attributes = Map("a" -> "b"))) { _ / "complex" / "moduleid.json" }
    //oneWayTrip(protocol.Stamps.empty) { _ / "complex" / "empty_stamp.json" }
    //oneWayTrip(protocol.APIs.empty) { _ / "complex" / "empty_apis.json" }
    //oneWayTrip(protocol.Relations.empty) { _ / "complex" / "empty_relations.json" }
    //oneWayTrip(protocol.SourceInfos.empty) { _ / "complex" / "empty_sourceinfos.json" }
    //oneWayTrip(protocol.Analysis.empty) { _ / "complex" / "empty_analysis.json" }
    oneWayTrip(new Exception(null, null)) { _ / "complex" / "empty_exception.json" }
    oneWayTrip(new Exception("fail fail fail", new RuntimeException("some cause"))) { _ / "complex" / "exception.json" }
    oneWayTrip(new protocol.CompileFailedException("the compile failed", null,
      Vector(protocol.Problem("something", xsbti.Severity.Error, "stuff didn't go well", FakePosition)))) { _ / "complex" / "compile_failed.json" }

    // message
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
    oneWayTrip[Message](protocol.ExecutionWaiting(41, "foo", protocol.ClientInfo("350954c2-6bf0-4925-b066-3bf20f32906b", "foo", "FOO"))) { _ / "event" / "exec_waiting.json" }
    oneWayTrip[Message](protocol.ExecutionStarting(56)) { _ / "event" / "exec_starting.json" }
    oneWayTrip[Message](protocol.ExecutionFailure(42)) { _ / "event" / "exec_failure.json" }
    oneWayTrip[Message](protocol.ExecutionSuccess(44)) { _ / "event" / "exec_success.json" }
    oneWayTrip[Message](protocol.TaskLogEvent(1, protocol.LogStdOut("Hello, world"))) { _ / "event" / "log" / "task_log_event.json" }
    oneWayTrip[Message](protocol.CoreLogEvent(protocol.LogStdOut("Hello, world"))) { _ / "event" / "log" / "core_log_event.json" }
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

    // stamp
    //val stamps = protocol.Stamps.empty.copy(allBinaries = Set(new File("/temp/foo.class")))
    //oneWayTrip[Stamp](protocol.Hash(ByteArray(Array(0.toByte)))) { _ / "stamp" / "hash.json" }
    //oneWayTrip[Stamp](protocol.LastModified(1)) { _ / "stamp" / "last_modified.json" }
    //oneWayTrip[Stamp](protocol.Exists(true)) { _ / "stamp" / "exists.json" }
    //oneWayTrip[Stamps](stamps) { _ / "stamp" / "stamps.json" }

    // source info
    //val problem = protocol.Problem("something", xsbti.Severity.Error, "stuff didn't go well", FakePosition)
    //val infos = protocol.SourceInfos(Map(new File("/temp/foo") -> protocol.SourceInfo(Vector(problem), Vector.empty)))
    //oneWayTrip(protocol.SourceInfo(Vector(problem), Vector.empty)) { _ / "source_info" / "source_info.json" }
    //oneWayTrip(infos) { _ / "source_info" / "source_infos.json" }

    // problem
    //oneWayTrip(problem) { _ / "problem" / "problem.json" }

    // apis
    //val outputSetting = protocol.OutputSetting("src", "target")
    //val compilation = protocol.Compilation(0, Vector(outputSetting))
    //val annoArg = protocol.AnnotationArgument("arg1", "1")
    //val annotation = protocol.Annotation(protocol.Singleton(protocol.Path(Vector(protocol.Id("annotation")))), Vector(annoArg))
    //val mods = protocol.Modifiers(false, false, false, false, false, false, false)
    //val definition = protocol.Definition("doSomething", protocol.Public,
    //  mods, Vector(annotation))
    //val thePackage = protocol.ThePackage("com.foo.Plugin")
    //val sourceApi = protocol.SourceAPI(Vector(thePackage), Vector(definition))
    //val source = protocol.Source(compilation, ByteArray(Array(0.toByte)), sourceApi, 0, false)
    //val apis = protocol.APIs(Set("foo"), Set(new File("/temp/foo")),
    //  Map(new File("/temp/foo") -> source),
    //  Map("foo" -> source))
    //oneWayTrip(apis) { _ / "apis" / "apis.json" }

    // the package
    //oneWayTrip(thePackage) { _ / "the_package" / "the_package.json" }

    // type parameters
    //val tpe = protocol.Singleton(protocol.Path(Vector(protocol.Id("Int"))))
    //val inv = xsbti.api.Variance.values()(2)
    //val typeParam = protocol.TypeParameter("A", Vector(annotation), Vector.empty, inv, tpe, tpe)
    //oneWayTrip(typeParam) { _ / "type_parameter" / "type_parameter.json" }

    // path
    //val path = protocol.Path(Vector(protocol.Id("Int")))
    //oneWayTrip(path) { _ / "path" / "path.json" }

    // modifiers
    //oneWayTrip(mods) { _ / "modifiers" / "modifiers.json" }

    // annotation argument
    //oneWayTrip(annoArg) { _ / "annotation_argument" / "annotation_argument.json" }

    // annotation
    //oneWayTrip(annotation) { _ / "annotation" / "annotation.json" }

    // definition
    //oneWayTrip(definition) { _ / "definition" / "definition.json" }

    // source API
    //oneWayTrip(sourceApi) { _ / "source_api" / "source_api.json" }

    // source
    //oneWayTrip(source) { _ / "source" / "source.json" }

    // relations source
    //val relationFf = protocol.Relation(Map(new File("/temp/foo") -> Set(new File("/temp/bar"))),
    //  Map(new File("/temp/bar") -> Set(new File("/temp/foo"))))
    //oneWayTrip(protocol.RelationsSource(
    //  internal = relationFf,
    //  external = protocol.Relation(Map(), Map()))) { _ / "relations_source" / "relations_source.json" }

    // relations
    //val relationFs = protocol.Relation(Map(new File("/temp/foo") -> Set("foo")),
    //  Map("foo" -> Set(new File("/temp/foo"))))
    //val relations = protocol.Relations(
    //  allSources = Set(new File("/temp/foo")),
    //  allProducts = Set(new File("/temp/target/foo.class")),
    //  allBinaryDeps = Set(new File("/temp/target/bar.class")),
    //  allInternalSrcDeps = Set(new File("/temp/bar")),
    //  allExternalDeps = Set("bar"),
    //  srcProd = relationFf,
    //  binaryDep = relationFf,
    //  internalSrcDep = relationFf,
    //  externalDep = relationFs,
    //  direct = None,
    //  publicInherited = None,
    //  classes = relationFs)
    //oneWayTrip(relations) { _ / "relations" / "relations.json" }

    // output setting
    //oneWayTrip(outputSetting) { _ / "output_setting" / "output_setting.json" }

    // compilation
    //oneWayTrip(compilation) { _ / "compilation" / "compilation.json" }
    //oneWayTrip(protocol.Compilations(Vector(compilation))) { _ / "compilation" / "compilations.json" }

    // access
    //oneWayTrip(protocol.Public: protocol.Access) { _ / "access" / "public.json" }
    //oneWayTrip(protocol.Protected(protocol.Unqualified): protocol.Access) { _ / "access" / "protected.json" }
    //oneWayTrip(protocol.Private(protocol.Unqualified): protocol.Access) { _ / "access" / "private.json" }
    //oneWayTrip(protocol.Protected(protocol.ThisQualifier): protocol.Access) { _ / "access" / "protected_this.json" }
    //oneWayTrip(protocol.Private(protocol.ThisQualifier): protocol.Access) { _ / "access" / "private_this.json" }
    //oneWayTrip(protocol.Protected(protocol.IdQualifier("foo")): protocol.Access) { _ / "access" / "protected_foo.json" }
    //oneWayTrip(protocol.Private(protocol.IdQualifier("foo")): protocol.Access) { _ / "access" / "private_foo.json" }

    // type
    //val foo_tpe = protocol.Singleton(protocol.Path(Vector(protocol.Id("Foo"))))
    //oneWayTrip(tpe: protocol.Type) { _ / "type" / "singleton.json" }
    //oneWayTrip(protocol.Singleton(protocol.Path(Vector(protocol.Super(protocol.Path(Vector(protocol.Id("Foo"))))))): protocol.Type) { _ / "type" / "super_foo.json" }
    //oneWayTrip(protocol.Singleton(protocol.Path(Vector(protocol.This))): protocol.Type) { _ / "type" / "singleton_this.json" }
    //oneWayTrip(protocol.Projection(protocol.Singleton(protocol.Path(Vector(protocol.This))), "Foo"): protocol.Type) { _ / "type" / "projection.json" }
    //oneWayTrip(
    //  protocol.Parameterized(foo_tpe,
    //    Vector(protocol.ParameterRef("A"))): protocol.Type) { _ / "type" / "parameterized.json" }
    //oneWayTrip(protocol.EmptyType: protocol.Type) { _ / "type" / "empty_type.json" }
    //oneWayTrip(protocol.Annotated(tpe, Vector(annotation)): protocol.Type) { _ / "type" / "annotated.json" }
    //oneWayTrip(protocol.Structure(Vector(tpe), Vector(definition), Vector(definition)): protocol.Type) { _ / "type" / "structure.json" }
    //oneWayTrip(protocol.Polymorphic(foo_tpe, Vector(typeParam)): protocol.Type) { _ / "type" / "polymorphic.json" }
    //oneWayTrip(protocol.Existential(foo_tpe, Vector(typeParam)): protocol.Type) { _ / "type" / "existential.json" }
    //oneWayTrip(protocol.Constant(foo_tpe, "A"): protocol.Type) { _ / "type" / "constant.json" }

    // byte array
    //oneWayTrip(ByteArray(Array(0.toByte))) { _ / "byte_array" / "byte_array.json" }

    // analysis
    // FIXME which fields do we need in analysis
    //oneWayTrip(protocol.Analysis()) { _ / "analysis" / "analysis.json" }
    /*    oneWayTrip(protocol.Analysis(
      stamps = stamps, apis = apis, relations = relations, infos = infos,
      compilations = protocol.Compilations(Vector(compilation)))) { _ / "analysis" / "analysis.json" }
*/
  }

  @Test
  def testSerializationStability(): Unit = {
    val baseDir = (new File("commons")) / "protocol" / "src" / "test" / "resource" / "saved-protocol"
    // uncomment this line to write new files
    oneWayTripTest(ObjectToJson, baseDir / "0.1")
    oneWayTripTest(JsonToObject, baseDir / "0.1")
  }

  @Test
  def testDynamicSerialization(): Unit = {
    val ds = DynamicSerialization.defaultSerializations
    def roundtripBase[U, T: Manifest](t: T)(f: (T, T) => U)(e: (Throwable, Throwable) => U): U = {
      val formatOption = ds.lookup(implicitly[Manifest[T]])
      formatOption map { format =>
        val json = addWhatWeWerePickling(t)(JsonValue(t)(format.pickler))
        //System.err.println(s"${t} = ${Json.prettyPrint(json)}")
        val parsed = addWhatWeWereUnpickling(json.renderCompact)(json.parse[T](format.unpickler).get)
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
      //System.err.println(s"${buildValue} = ${Json.prettyPrint(json)}")
      val parsedValue = json.parse[protocol.BuildValue].getOrElse(throw new AssertionError(s"Failed to parse ${t} serialization ${json}"))
      val parsedT = parsedValue.value[T](format.unpickler).getOrElse(throw new AssertionError(s"could not read back from build value ${t.getClass.getName} $t"))
      val buildValueClass: Class[_] = t.getClass
      // Throwable has a not-very-useful equals() in this case
      if (classOf[Throwable].isAssignableFrom(buildValueClass)) {
        val p = parsedValue.value[T](format.unpickler).map(_.asInstanceOf[Throwable]).get
        e(t.asInstanceOf[Throwable], p)
      } else {
        f(buildValue, parsedValue)
      }
    }

    def roundtripBuildValue[T: Manifest](t: T): Unit =
      roundtripBuild[Unit, T](t)((x, y) => assertEquals("round trip of Serialized( " + implicitly[Manifest[T]] + ") [" + t + "]", x, y))((t, p) => assertEquals("round trip of message " + t.getMessage, t.getMessage, p.getMessage))

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
