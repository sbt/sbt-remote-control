/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import sbt.protocol
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import play.api.libs.json._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

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

  private case class ConcretePosition(
    line: xsbti.Maybe[Integer],
    lineContent: String,
    offset: xsbti.Maybe[Integer],
    pointer: xsbti.Maybe[Integer],
    pointerSpace: xsbti.Maybe[String],
    sourcePath: xsbti.Maybe[String],
    sourceFile: xsbti.Maybe[java.io.File]) extends xsbti.Position

  private def toInteger(in: Int): Integer = new java.lang.Integer(in)

  implicit val arbitraryPosition: Arbitrary[xsbti.Position] = Arbitrary {
    for {
      line <- genMaybe(arbitrary[Int].map(toInteger))
      lineContent <- Gen.alphaStr
      offset <- genMaybe(arbitrary[Int].map(toInteger))
      pointer <- genMaybe(arbitrary[Int].map(toInteger))
      pointerSpace <- genMaybe(Gen.alphaStr)
      sourcePath <- genMaybe(Gen.alphaStr)
      sourceFile <- genMaybe(arbitraryFile.arbitrary)
    } yield protocol.fromXsbtiPosition(ConcretePosition(line,
      lineContent,
      offset,
      pointer,
      pointerSpace,
      sourcePath,
      sourceFile))
  }

  implicit val arbitrarySeverity: Arbitrary[xsbti.Severity] = Arbitrary(Gen.oneOf(xsbti.Severity.values.toSeq))

  implicit val arbitraryProblem: Arbitrary[xsbti.Problem] = Arbitrary {
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
    } yield protocol.SourceInfo(reportedProblems, unreportedProblems)
  }

  implicit val arbitrarySourceInfos: Arbitrary[protocol.SourceInfos] =
    Arbitrary(genMap(3, Gen.zip(arbitraryFile.arbitrary, arbitrarySourceInfo.arbitrary)).map(protocol.SourceInfos.apply))

  implicit val arbitraryOutputSetting: Arbitrary[protocol.OutputSetting] =
    Arbitrary(Gen.zip(Gen.alphaStr, Gen.alphaStr).map { case (a, b) => protocol.OutputSetting(a, b) })

  implicit val arbitraryCompilation: Arbitrary[protocol.Compilation] = Arbitrary {
    for {
      time <- arbitrary[Long]
      settings <- listOfN()(0, 10, arbitraryOutputSetting.arbitrary)
    } yield protocol.Compilation(time, settings)
  }

  implicit val arbitraryCompilations: Arbitrary[protocol.Compilations] = Arbitrary {
    for {
      compilations <- listOfN()(0, 10, arbitraryCompilation.arbitrary)
    } yield protocol.Compilations(compilations)
  }

  def genAnalysis(maxDepth: Int = 3): Gen[protocol.Analysis] =
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
    } yield protocol.SourceAPI(packages, definitions)

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
    } yield protocol.Path(path)

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
      annotations)

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
      annotations,
      typeParameters,
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
    } yield protocol.Parameterized(st, args)
    val genParameterRef = Gen.alphaStr.map(protocol.ParameterRef.apply)

    if (maxDepth > 0) Gen.oneOf(Gen.const(protocol.EmptyType), genSingleton, genParameterRef, genProjection, genParameterized)
    else Gen.const(protocol.EmptyType)
  }

  def genType(maxDepth: Int = 3): Gen[protocol.Type] = {
    lazy val genAnnotated = for {
      typ <- genType(maxDepth - 1)
      annotations <- listOfN(maxDepth - 1)(0, 2, genAnnotation(maxDepth - 1))
    } yield protocol.Annotated(typ, annotations)
    lazy val genStructure = for {
      parents <- listOfN(maxDepth - 1)(0, 2, genType(maxDepth - 1))
      declared <- listOfN(maxDepth - 1)(0, 2, genDefinition(maxDepth - 1))
      inherited <- listOfN(maxDepth - 1)(0, 2, genDefinition(maxDepth - 1))
    } yield protocol.Structure(parents, declared, inherited)
    lazy val genPolymorphic = for {
      typ <- genType(maxDepth - 1)
      typeParameters <- listOfN(maxDepth - 1)(0, 2, genTypeParameter(maxDepth - 1))
    } yield protocol.Polymorphic(typ, typeParameters)
    lazy val genExistential = for {
      typ <- genType(maxDepth - 1)
      typeParameters <- listOfN(maxDepth - 1)(0, 2, genTypeParameter(maxDepth - 1))
    } yield protocol.Existential(typ, typeParameters)
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
    } yield protocol.Annotation(typ, args)

}

final case class PlayStartedEvent(port: Int)
object PlayStartedEvent extends protocol.TaskEventUnapply[PlayStartedEvent] {
  implicit val format = Json.format[PlayStartedEvent]
}

class ProtocolTest {
  import ProtocolGenerators._

  private def addWhatWeWereFormatting[T, U](t: T)(body: => U): U = try body
  catch {
    case e: Throwable =>
      throw new AssertionError(s"Crash formatting ${t.getClass.getName}: ${e.getMessage}", e)
  }

  @Test
  def testRawStructure(): Unit = {
    val key = protocol.AttributeKey("name", protocol.TypeInfo("java.lang.String"))
    val build = new java.net.URI("file:///test/project")
    val scope = protocol.SbtScope(project = Some(
      protocol.ProjectReference(build, "test")))
    val scopedKey = protocol.ScopedKey(key, scope)
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Seq(build),
      projects = Seq(protocol.MinimalProjectStructure(scope.project.get, Seq("com.foo.Plugin"))))

    val serializations = protocol.DynamicSerialization.defaultSerializations

    val specifics = Seq(
      // Requests
      protocol.KillServerRequest(),
      protocol.ReadLineRequest(42, "HI", true),
      protocol.ReadLineResponse(Some("line")),
      protocol.ConfirmRequest(43, "msg"),
      protocol.ConfirmResponse(true),
      protocol.ReceivedResponse(),
      protocol.RequestCompleted(),
      protocol.CommandCompletionsRequest("He", 2),
      protocol.CommandCompletionsResponse(Set(protocol.Completion("llo", "Hello", true))),
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
      protocol.TaskFinished(48, 1, Some(scopedKey), true),
      protocol.TaskStarted(47, 1, None),
      protocol.TaskFinished(48, 1, None, true),
      protocol.TaskStarted(49, 2, Some(scopedKey)),
      protocol.TaskFinished(50, 2, Some(scopedKey), true),
      protocol.BuildStructureChanged(buildStructure),
      // equals() doesn't work on Exception so we can't actually check this easily
      //protocol.ValueChanged(scopedKey, protocol.TaskFailure("O NOES", protocol.BuildValue(new Exception("Exploded"), serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue("HI", serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(42, serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(43L, serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(true, serializations))),
      // TODO make Unit work ?
      // protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(()))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0, serializations))),
      protocol.ValueChanged(scopedKey, protocol.TaskSuccess(protocol.BuildValue(0.0f, serializations))),
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
      protocol.BackgroundJobEvent(67, PlayStartedEvent(port = 10)))

    for (s <- specifics) {
      import protocol.WireProtocol.{ fromRaw, toRaw }
      val roundtrippedOption = addWhatWeWereFormatting(s)(fromRaw(toRaw(s), protocol.DynamicSerialization.defaultSerializations))
      assertEquals(s"Failed to serialize:\n$s\n\n${toRaw(s)}\n\n", Some(s), roundtrippedOption)
    }

    protocol.TaskEvent(4, protocol.TestEvent("name", Some("foo"), protocol.TestPassed, Some("bar"), 0)) match {
      case protocol.TestEvent(taskId, test) =>
        assertEquals(4, taskId)
        assertEquals("name", test.name)
        assertEquals(Some("foo"), test.description)
        assertEquals(Some("bar"), test.error)
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

  private def roundtripTest(roundtripper: Roundtripper): Unit = {
    val ds = protocol.DynamicSerialization.defaultSerializations

    def roundtrip[T: Manifest](t: T): Unit = roundtripper.roundtrip(t)
    def roundtripPropTest[T: Manifest](t: T): Boolean = roundtripper.roundtripPropTest(t)

    val key = protocol.AttributeKey("name", protocol.TypeInfo("java.lang.String"))
    val build = new java.net.URI("file:///test/project")
    val projectRef = protocol.ProjectReference(build, "test")
    val scope = protocol.SbtScope(project = Some(projectRef))
    val scopedKey = protocol.ScopedKey(key, scope)
    val buildStructure = protocol.MinimalBuildStructure(
      builds = Seq(build),
      projects = Seq(protocol.MinimalProjectStructure(scope.project.get, Seq("com.foo.Plugin"))))
    object FakePosition extends xsbti.Position {
      override def line = xsbti.Maybe.just(10)
      override def offset = xsbti.Maybe.just(11)
      override def pointer = xsbti.Maybe.just(12)
      override def pointerSpace = xsbti.Maybe.just("foo")
      override def sourcePath = xsbti.Maybe.just("foo")
      override def sourceFile = xsbti.Maybe.just(new java.io.File("bar"))
      override def lineContent = "this is some stuff on the line"
      // note, this does not have a useful equals/hashCode with other instances of Position
    }

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
    roundtrip(Nil: Seq[String])
    roundtrip(Seq("Bar", "Baz"))
    roundtrip(Seq(1, 2, 3))
    roundtrip(Seq(true, false, true, true, false))
    roundtrip(key)
    roundtrip(build)
    roundtrip(projectRef)
    roundtrip(scope)
    roundtrip(scopedKey)
    roundtrip(buildStructure)
    roundtrip(protocol.ModuleId(organization = "com.foo", name = "bar", attributes = Map("a" -> "b")))
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
      property("xsbti.Problem") = forAll { x: xsbti.Problem => roundtripPropTest(x) }
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
    roundtrip(protocol.Analysis.empty)
    roundtrip(new Exception(null, null))
    roundtrip(new Exception("fail fail fail", new RuntimeException("some cause")))
    roundtrip(new protocol.CompileFailedException("the compile failed", null,
      Seq(protocol.Problem("something", xsbti.Severity.Error, "stuff didn't go well", FakePosition))))
  }

  @Test
  def testDynamicSerialization(): Unit = {
    val ds = protocol.DynamicSerialization.defaultSerializations
    def roundtripBase[U, T: Manifest](t: T)(f: (T, T) => U)(e: (Throwable, Throwable) => U): U = {
      val formatOption = ds.lookup(implicitly[Manifest[T]])
      formatOption map { format =>
        val json = addWhatWeWereFormatting(t)(format.writes(t))
        //System.err.println(s"${t} = ${Json.prettyPrint(json)}")
        val parsed = addWhatWeWereFormatting(t)(format.reads(json)).asOpt.getOrElse(throw new AssertionError(s"could not re-parse ${json} for ${t}"))
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
    val serializations = protocol.DynamicSerialization.defaultSerializations
    implicit def format[T]: Format[protocol.BuildValue[T]] = protocol.BuildValue.format[T](serializations)
    def roundtripBuild[U, T: Manifest](buildValue: protocol.BuildValue[T])(f: (protocol.BuildValue[T], protocol.BuildValue[T]) => U)(e: (Throwable, Throwable) => U): U = {
      val json = Json.toJson(buildValue)
      //System.err.println(s"${buildValue} = ${Json.prettyPrint(json)}")
      val parsed = Json.fromJson[protocol.BuildValue[T]](json).asOpt.getOrElse(throw new AssertionError(s"Failed to parse ${buildValue} serialization ${json}"))
      val buildValueClass: Class[_] = buildValue match {
        case bv: protocol.SerializableBuildValue[T] => bv.manifest.toManifest(this.getClass.getClassLoader)
          .map(_.runtimeClass).getOrElse(throw new AssertionError("don't have class for this build value"))
        case other => throw new AssertionError("non-serializable build value $other")
      }
      // Throwable has a not-very-useful equals() in this case
      if (classOf[Throwable].isAssignableFrom(buildValueClass)) {
        val t = buildValue.value.map(_.asInstanceOf[Throwable]).get
        val p = parsed.value.map(_.asInstanceOf[Throwable]).get
        e(t, p)
      } else {
        f(buildValue, parsed)
      }
    }

    def roundtripBuildValue[T: Manifest](buildValue: protocol.BuildValue[T]): Unit =
      roundtripBuild[Unit, T](buildValue)(assertEquals)((t, p) => assertEquals("round trip of message " + t.getMessage, t.getMessage, p.getMessage))

    def roundtripBuildValueTest[T: Manifest](buildValue: protocol.BuildValue[T]): Boolean =
      roundtripBuild[Boolean, T](buildValue) { (a, b) =>
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
        roundtripBuildValue(protocol.BuildValue(t, serializations))
      }
      override def roundtripPropTest[T: Manifest](t: T): Boolean = {
        roundtripBuildValueTest(protocol.BuildValue(t, serializations))
      }
    })
  }
}
