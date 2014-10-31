package sbt

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import protocol.JsonHelpers._

// These serializers are generic or weird/hacky so don't belong
// in sbt.protocol. Moving some of them to protocol also creates circular
// initialization problems.
object GenericSerializers {

  // adapt the Int reads/writes to boxed java.lang.Integer
  implicit val integerReads = Reads[java.lang.Integer](j => implicitly[Reads[Int]].reads(j).map(new java.lang.Integer(_)))
  implicit val integerWrites = Writes[java.lang.Integer](i => implicitly[Writes[Int]].writes(i))

  def fileFromString(s: String): Option[java.io.File] =
    try Some(new java.io.File(new java.net.URI(s)))
    catch {
      case e: Exception => None
    }
  def fileToString(f: java.io.File): String =
    f.toURI.toASCIIString

  implicit val fileReads = Reads[java.io.File] { j =>
    j.validate[String].flatMap(x =>
      fileFromString(x).map(JsSuccess(_)).getOrElse(JsError(s"Invalid filename $x")))
  }
  implicit val fileWrites = Writes[java.io.File](f => JsString(fileToString(f)))

  implicit val uriReads = Reads[java.net.URI](v => v.validate[String].map(x => new java.net.URI(x)))
  implicit val uriWrites = Writes[java.net.URI](u => JsString(u.toASCIIString))

  // this is not implicit because it would cause trouble with
  // more specific formatters; we just use it as an explicit fallback
  val throwableReads: Reads[java.lang.Throwable] = Reads[java.lang.Throwable] { v =>
    implicit def recursiveReads = throwableReads
    def validateOrNull[T <: AnyRef](json: JsValue)(implicit r: Reads[T]): JsResult[T] = json match {
      case JsNull => JsSuccess(null.asInstanceOf[T])
      case _ => r.reads(json)
    }
    for {
      message <- validateOrNull[String](v \ "message")
      cause <- validateOrNull[Throwable](v \ "cause")
    } yield new Exception(message, cause)
  }
  val throwableWrites: OWrites[java.lang.Throwable] = OWrites[java.lang.Throwable] { t =>
    implicit def recursiveWrites = throwableWrites
    JsObject(Seq("message" -> Option(t.getMessage).map(JsString(_)).getOrElse(JsNull),
      "cause" -> Option(t.getCause).map(Json.toJson(_)).getOrElse(JsNull)))
  }

}

package object protocol {
  // default serializers
  import Reads._
  import Writes._
  import GenericSerializers._

  implicit def attributedWrites[T](implicit writes: Writes[T]) = Writes[Attributed[T]] { t =>
    JsObject(Seq("attributed" -> JsBoolean(true),
      "data" -> writes.writes(t.data)))
  }

  implicit def attributedReads[T](implicit reads: Reads[T]) = Reads[Attributed[T]] { obj =>
    (obj \ "attributed") match {
      case JsBoolean(true) =>
        reads.reads(obj \ "data").map(sbt.Attributed.blank)
      case _ => JsError("not a valid attributed.")
    }
  }

  implicit val severityWrites = Writes[xsbti.Severity](s => JsString(s.toString))
  implicit val severityReads = Reads[xsbti.Severity] { in =>
    in match {
      case JsString(s) => Option(xsbti.Severity.valueOf(s)).map(sev => JsSuccess(sev)).getOrElse(JsError("Could not find severity: " + s))
      case _ => JsError("Could not find severity: " + in)
    }
  }

  private def convert[T](o: Option[T]): xsbti.Maybe[T] =
    o match {
      case Some(value) => xsbti.Maybe.just(value)
      case None => xsbti.Maybe.nothing()
    }

  private def convertToOption[T](o: xsbti.Maybe[T]): Option[T] =
    if (o.isDefined()) Some(o.get())
    else None

  def defineIf[T](value: xsbti.Maybe[T], name: String)(implicit writes: Writes[T]): Seq[(String, JsValue)] =
    if (value.isDefined) Seq(name -> writes.writes(value.get)) else Nil

  private final case class PositionDeserialized(lineContent: String, l: Option[Int], o: Option[Int], p: Option[Int],
    ps: Option[String], sp: Option[String]) extends xsbti.Position {
    override def line = convert(l.map(Integer.valueOf))
    override def offset = convert(o.map(Integer.valueOf))
    override def pointer = convert(p.map(Integer.valueOf))
    override def pointerSpace = convert(ps)
    override def sourcePath = convert(sp)
    override def sourceFile = convert(sp.map(new java.io.File(_)))
    override def equals(o: Any): Boolean = o match {
      case pos: xsbti.Position => protocol.StructurallyEqual.equals(this, pos)
      case _ => false
    }
  }

  def fromXsbtiPosition(in: xsbti.Position): xsbti.Position =
    PositionDeserialized(in.lineContent(),
      convertToOption(in.line()).map(_.intValue),
      convertToOption(in.offset()).map(_.intValue),
      convertToOption(in.pointer()).map(_.intValue),
      convertToOption(in.pointerSpace()),
      convertToOption(in.sourcePath()))

  implicit val positionReads: Reads[xsbti.Position] = (
    (__ \ "lineContent").read[String] and
    (__ \ "line").readNullable[Int] and
    (__ \ "offset").readNullable[Int] and
    (__ \ "pointer").readNullable[Int] and
    (__ \ "pointerSpace").readNullable[String] and
    (__ \ "sourcePath").readNullable[String])(PositionDeserialized.apply _)

  implicit val positionWrites: Writes[xsbti.Position] = Writes[xsbti.Position] { in =>
    val line = defineIf(in.line, "line")
    val offset = defineIf(in.offset, "offset")
    val pointer = defineIf(in.pointer, "pointer")
    val pointerSpace = defineIf(in.pointerSpace, "pointerSpace")
    val sourcePath = defineIf(in.sourcePath, "sourcePath")
    val sourceFile = defineIf(in.sourceFile, "sourceFile")
    JsObject(Seq("lineContent" -> JsString(in.lineContent)) ++
      line ++
      offset ++
      pointer ++
      pointerSpace ++
      sourcePath ++
      sourceFile)
  }

  implicit val testOutcomeReads =
    Reads[TestOutcome] { value =>
      value.validate[String].flatMap {
        case "passed" => JsSuccess(TestPassed)
        case "failed" => JsSuccess(TestFailed)
        case "error" => JsSuccess(TestError)
        case "skipped" => JsSuccess(TestSkipped)
        case other => JsError(s"Unknown test outcome - $other")
      }
    }
  implicit val testOutcomeWrites =
    Writes[TestOutcome](outcome => JsString(outcome.toString))
  implicit val testGroupResultReads =
    Reads[TestGroupResult] { value =>
      value.validate[String].flatMap {
        case "passed" => JsSuccess(TestGroupPassed)
        case "failed" => JsSuccess(TestGroupFailed)
        case "error" => JsSuccess(TestGroupError)
        case other => JsError(s"Unknown test group result - $other")
      }
    }
  implicit val testGroupResultWrites =
    Writes[TestGroupResult](result => JsString(result.toString))

  ///// task events (do not extend protocol.Message)
  // these formatters are hand-coded because they have an unapply()
  // that confuses play-json

  implicit val testGroupStartedReads: Reads[TestGroupStarted] =
    (__ \ "name").read[String].map(TestGroupStarted(_))
  implicit val testGroupStartedWrites: Writes[TestGroupStarted] =
    (__ \ "name").write[String].contramap(x => x.name)

  implicit val testGroupFinishedReads: Reads[TestGroupFinished] = (
    (__ \ "name").read[String] and
    (__ \ "result").read[TestGroupResult] and
    (__ \ "error").readNullable[String])(TestGroupFinished.apply _)
  implicit val testGroupFinishedWrites: Writes[TestGroupFinished] = (
    (__ \ "name").write[String] and
    (__ \ "result").write[TestGroupResult] and
    (__ \ "error").writeNullable[String])(unlift(TestGroupFinished.unapply))

  implicit val testEventReads: Reads[TestEvent] = (
    (__ \ "name").read[String] and
    (__ \ "description").readNullable[String] and
    (__ \ "outcome").read[TestOutcome] and
    (__ \ "error").readNullable[String] and
    (__ \ "duration").read[Long])(TestEvent.apply _)

  implicit val testEventWrites: Writes[TestEvent] = Writes[TestEvent] { event =>
    Json.obj("name" -> event.name, "description" -> event.description,
      "outcome" -> event.outcome, "error" -> event.error, "duration" -> event.duration)
  }

  implicit val compilationFailureReads: Reads[CompilationFailure] = (
    (__ \ "project").read[ProjectReference] and
    (__ \ "position").read[xsbti.Position] and
    (__ \ "severity").read[xsbti.Severity] and
    (__ \ "message").read[String])(CompilationFailure.apply _)

  implicit val compilationFailureWrites: Writes[CompilationFailure] = Writes[CompilationFailure] { event =>
    Json.obj("project" -> event.project, "position" -> event.position,
      "severity" -> event.severity, "message" -> event.message)
  }

  implicit val immutableByteArrayReads: Reads[ByteArray] = Reads(_.asOpt[String] map { in =>
    try {
      val r = hexToBytes(in)
      JsSuccess(ByteArray(r))
    } catch {
      case e: Exception => JsError(Seq(JsPath() -> Seq(ValidationError(s"Could not decode string '$in' as hex."))))
    }
  } getOrElse JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected.jsstring")))))
  implicit val immutableByteArrayWrites: Writes[ByteArray] = Writes(in => JsString(bytesToHex(in.toArray)))

  implicit def fileMapReads[T](implicit tReads: Reads[T]): Reads[Map[java.io.File, T]] = Reads[Map[java.io.File, T]] { json =>
    val stringMapReads = implicitly[Reads[Map[String, T]]]
    stringMapReads.reads(json) flatMap { stringMap =>
      try JsSuccess(stringMap.map(kv => fileFromString(kv._1).getOrElse(throw new Exception(s"invalid filename ${kv._1}")) -> kv._2))
      catch {
        case e: Exception => JsError(e.getMessage)
      }
    }
  }

  implicit def fileMapWrites[T](implicit tWrites: Writes[T]): OWrites[Map[java.io.File, T]] = OWrites[Map[java.io.File, T]] { m =>
    val stringMapWrites = implicitly[OWrites[Map[String, T]]]
    val stringMap = m.map(kv => fileToString(kv._1) -> kv._2)
    stringMapWrites.writes(stringMap)
  }

  implicit def relationReads[A, B](implicit forwardReads: Reads[Map[A, Set[B]]], reverseReads: Reads[Map[B, Set[A]]]): Reads[Relation[A, B]] = Reads[Relation[A, B]] { json =>
    ((__ \ "forwardMap").read[Map[A, Set[B]]] and
      (__ \ "reverseMap").read[Map[B, Set[A]]]).apply(Relation(_, _)).reads(json)
  }
  implicit def relationWrites[A, B](implicit forwardWrites: OWrites[Map[A, Set[B]]], reverseWrites: OWrites[Map[B, Set[A]]]): Writes[Relation[A, B]] = OWrites[Relation[A, B]] { in =>
    Json.obj("forwardMap" -> in.forwardMap, "reverseMap" -> in.reverseMap)
  }

  implicit val stampReads: Reads[Stamp] = new Reads[Stamp] {
    def reads(json: JsValue): JsResult[Stamp] =
      (json \ "type").validate[String].flatMap {
        _ match {
          case "hash" => (json \ "value").validate[ByteArray].map(Hash(_))
          case "lastModified" => (json \ "value").validate[Long].map(LastModified(_))
          case "exists" => (json \ "value").validate[Boolean].map(Exists(_))
          case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'hash', 'lastModified', or 'exists', got: $x"))))
        }
      }
  }
  implicit val stampWrites: Writes[Stamp] = new Writes[Stamp] {
    def writes(o: Stamp): JsValue = o match {
      case x: Hash => emitTypedValue("hash", "value" -> x.value)
      case x: LastModified => emitTypedValue("lastModified", "value" -> x.value)
      case x: Exists => emitTypedValue("exists", "value" -> x.value)
    }
  }

  implicit val stampsReads = Json.reads[Stamps]
  implicit val stampsWrites = Json.writes[Stamps]

  implicit val problemReads: Reads[Problem] = Json.reads[Problem]
  // this one causes ambiguity trouble with Writes[xsbti.Problem] and isn't needed
  // implicitly since the xsbti.Problem writes is just fine.
  private val problemWrites: Writes[Problem] = Json.writes[Problem]

  implicit val xsbtiProblemReads = Reads[xsbti.Problem](problemReads.reads)
  implicit val xsbtiProblemWrites = Writes[xsbti.Problem](x => problemWrites.writes(Problem.fromXsbtiProblem(x)))

  implicit val qualifierReads: Reads[Qualifier] = new Reads[Qualifier] {
    def reads(json: JsValue): JsResult[Qualifier] =
      (json \ "type").validate[String].flatMap {
        _ match {
          case "this" => JsSuccess(ThisQualifier)
          case "unqualified" => JsSuccess(Unqualified)
          case "id" => (json \ "value").validate[String].map(IdQualifier(_))
          case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'this', 'unqualified', or 'id', got: $x"))))
        }
      }
  }
  implicit val qualifierWrites: Writes[Qualifier] = new Writes[Qualifier] {
    def writes(o: Qualifier): JsValue = o match {
      case ThisQualifier => emitTypedValue("this")
      case Unqualified => emitTypedValue("unqualified")
      case x: IdQualifier => emitTypedValue("id", "value" -> x.value)
    }
  }

  implicit val accessWrites = Writes[Access] { o =>
    o match {
      case Public => emitTypedValue("public")
      case x: Protected => emitTypedValue("protected", "qualifier" -> x.qualifier)
      case x: Private => emitTypedValue("private", "qualifier" -> x.qualifier)
    }
  }
  implicit val accessReads = Reads[Access] { json =>
    (json \ "type").validate[String].flatMap {
      _ match {
        case "public" => JsSuccess(Public)
        case "protected" => (json \ "qualifier").validate[Qualifier].map(Protected(_))
        case "private" => (json \ "qualifier").validate[Qualifier].map(Private(_))
        case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'public', 'protected', or 'private', got: $x"))))
      }
    }
  }

  implicit val varianceWrites = Writes[xsbti.api.Variance] { o =>
    import xsbti.api.Variance._
    o match {
      case Invariant => emitTypedValue("invariant")
      case Covariant => emitTypedValue("covariant")
      case Contravariant => emitTypedValue("contravariant")
    }
  }
  implicit val varianceReads = Reads[xsbti.api.Variance] { json =>
    import xsbti.api.Variance._
    (json \ "type").validate[String].flatMap {
      _ match {
        case "invariant" => JsSuccess(Invariant)
        case "covariant" => JsSuccess(Covariant)
        case "contravariant" => JsSuccess(Contravariant)
        case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'invariant', 'covariant', or 'contravariant', got: $x"))))
      }
    }
  }
  // lazy needed to avoid NPE
  implicit lazy val pathComponentWrites = Writes[PathComponent] { o =>
    o match {
      case x: Id => emitTypedValue("id", "id" -> x.id)
      case x: Super => emitTypedValue("super", "qualifier" -> x.qualifier)
      case This => emitTypedValue("this")
    }
  }
  implicit lazy val pathComponentReads = Reads[PathComponent] { json =>
    (json \ "type").validate[String].flatMap {
      _ match {
        case "id" => (json \ "id").validate[String].map(Id(_))
        case "super" => (json \ "qualifier").validate[Path].map(Super(_))
        case "this" => JsSuccess(This)
        case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'id', 'super', or 'this', got: $x"))))
      }
    }
  }
  // lazy needed to avoid NPE
  implicit lazy val pathReads: Reads[Path] = Json.reads[Path]
  implicit lazy val pathWrites: OWrites[Path] = Json.writes[Path]
  // lazy needed to avoid NPE
  implicit lazy val typeParameterReads: Reads[TypeParameter] = Json.reads[TypeParameter]
  implicit lazy val typeParameterWrites: Writes[TypeParameter] = Json.writes[TypeParameter]

  // lazy needed to avoid NPE
  implicit lazy val simpleTypeReads: Reads[SimpleType] = new Reads[SimpleType] {
    def reads(json: JsValue): JsResult[SimpleType] =
      (json \ "type").validate[String].flatMap {
        _ match {
          case "singleton" => (json \ "path").validate[Path].map(Singleton(_))
          case "projection" => for {
            prefix <- (json \ "prefix").validate[SimpleType]
            id <- (json \ "id").validate[String]
          } yield Projection(prefix, id)
          case "parameterized" => for {
            baseType <- (json \ "baseType").validate[SimpleType]
            typeArguments <- (json \ "typeArguments").validate[Seq[Type]]
          } yield Parameterized(baseType, typeArguments)
          case "parameterRef" => (json \ "id").validate[String].map(ParameterRef(_))
          case "emptyType" => JsSuccess(EmptyType)
          case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'singleton', 'projection', 'parameterized', or 'emptyType', got: $x"))))
        }
      }
  }
  // This one causes ambiguity with Writes[Type] and isn't needed as a public implicit
  // because Writes[Type] works fine.
  private lazy val simpleTypeWrites: Writes[SimpleType] = new Writes[SimpleType] {
    def writes(o: SimpleType): JsValue = o match {
      case x: Singleton => emitTypedValue("singleton", "path" -> x.path)
      case x: Projection => emitTypedValue("projection", "prefix" -> writes(x.prefix), "id" -> x.id)
      case x: Parameterized => emitTypedValue("parameterized", "baseType" -> writes(x.baseType), "typeArguments" -> x.typeArguments)
      case x: ParameterRef => emitTypedValue("parameterRef", "id" -> x.id)
      case EmptyType => emitTypedValue("emptyType")
    }
  }
  // lazy needed to avoid NPE
  implicit lazy val typeReads: Reads[Type] = new Reads[Type] {
    def reads(json: JsValue): JsResult[Type] =
      (json \ "type").validate[String].flatMap {
        _ match {
          case "annotated" => for {
            baseType <- (json \ "baseType").validate[Type]
            annotations <- (json \ "annotations").validate[Seq[Annotation]]
          } yield Annotated(baseType, annotations)
          case "structure" => for {
            parents <- (json \ "parents").validate[Seq[Type]]
            declared <- (json \ "declared").validate[Seq[Definition]]
            inherited <- (json \ "inherited").validate[Seq[Definition]]
          } yield Structure(parents, declared, inherited)
          case "polymorphic" => for {
            baseType <- (json \ "baseType").validate[Type]
            parameters <- (json \ "parameters").validate[Seq[TypeParameter]]
          } yield Polymorphic(baseType, parameters)
          case "existential" => for {
            baseType <- (json \ "baseType").validate[Type]
            clause <- (json \ "clause").validate[Seq[TypeParameter]]
          } yield Existential(baseType, clause)
          case "constant" => for {
            baseType <- (json \ "baseType").validate[Type]
            value <- (json \ "value").validate[String]
          } yield Constant(baseType, value)
          case x => simpleTypeReads.reads(json) match {
            case s: JsSuccess[SimpleType] => s
            case _: JsError => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'annotated', 'structure', 'polymorphic', 'existential', 'constant', 'singleton', 'projection', 'parameterized', or 'emptyType', got: $x"))))
          }
        }
      }
  }
  // lazy needed to avoid NPE
  implicit lazy val typeWrites: Writes[Type] = new Writes[Type] {
    def writes(o: Type): JsValue = o match {
      case x: SimpleType => simpleTypeWrites.writes(x)
      case x: Annotated => emitTypedValue("annotated", "baseType" -> x.baseType, "annotations" -> x.annotations)
      case x: Structure => emitTypedValue("structure", "parents" -> x.parents, "declared" -> x.declared, "inherited" -> x.inherited)
      case x: Polymorphic => emitTypedValue("polymorphic", "baseType" -> x.baseType, "parameters" -> x.parameters)
      case x: Existential => emitTypedValue("existential", "baseType" -> x.baseType, "clause" -> x.clause)
      case x: Constant => emitTypedValue("constant", "baseType" -> x.baseType, "value" -> x.value)
    }
  }

  // lazy needed to avoid NPE
  implicit lazy val annotationArgumentReads = Json.reads[AnnotationArgument]
  implicit lazy val annotationArgumentWrites = Json.writes[AnnotationArgument]
  // lazy needed to avoid NPE
  implicit lazy val annotationReads = Json.reads[Annotation]
  implicit lazy val annotationWrites = Json.writes[Annotation]
  implicit val packageReads = Json.reads[ThePackage]
  implicit val packageWrites = Json.writes[ThePackage]
  implicit val sourceInfoReads = Json.reads[SourceInfo]
  implicit val sourceInfoWrites = Json.writes[SourceInfo]
  implicit val sourceInfosReads = Json.reads[SourceInfos]
  implicit val sourceInfosWrites = Json.writes[SourceInfos]
  implicit val outputSettingReads = Json.reads[OutputSetting]
  implicit val outputSettingWrites = Json.writes[OutputSetting]
  implicit val modifiersReads = Json.reads[Modifiers]
  implicit val modifiersWrites = Json.writes[Modifiers]
  // lazy needed to avoid NPE
  implicit lazy val definitionReads = Json.reads[Definition]
  implicit lazy val definitionWrites = Json.writes[Definition]
  implicit val compilationReads = Json.reads[Compilation]
  implicit val compilationWrites = Json.writes[Compilation]
  implicit val sourceAPIReads = Json.reads[SourceAPI]
  implicit val sourceAPIWrites = Json.writes[SourceAPI]
  implicit val sourceReads = Json.reads[Source]
  implicit val sourceWrites = Json.writes[Source]
  implicit val apisReads = Json.reads[APIs]
  implicit val apisWrites = Json.writes[APIs]
  implicit val compilationsReads = Json.reads[Compilations]
  implicit val compilationsWrites = Json.writes[Compilations]
  implicit val relationsSourceReads = Json.reads[RelationsSource]
  implicit val relationsSourceWrites = Json.writes[RelationsSource]
  implicit val relationsReads = Json.reads[Relations]
  implicit val relationsWrites = Json.writes[Relations]
  implicit val analysisReads = Json.reads[Analysis]
  implicit val analysisWrites = Json.writes[Analysis]

  implicit val compileFailedExceptionReads: Reads[CompileFailedException] = new Reads[CompileFailedException] {
    override def reads(json: JsValue): JsResult[CompileFailedException] = {
      for {
        t <- throwableReads.reads(json)
        problems <- (json \ "problems").validate[Seq[xsbti.Problem]]
      } yield new CompileFailedException(t.getMessage, t.getCause, problems)
    }
  }

  implicit val compileFailedExceptionWrites: Writes[CompileFailedException] = new Writes[CompileFailedException] {
    override def writes(e: CompileFailedException): JsValue = {
      val json = throwableWrites.writes(e)
      json ++ Json.obj("problems" -> e.problems)
    }
  }

  implicit val compileFailedExceptionFormat: Format[CompileFailedException] =
    Format(compileFailedExceptionReads, compileFailedExceptionWrites)

  implicit val moduleIdReads: Reads[ModuleId] = Json.reads[ModuleId]
  implicit val moduleIdWrites: Writes[ModuleId] = Json.writes[ModuleId]
}
