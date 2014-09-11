package sbt

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import protocol.JsonHelpers._

package object protocol {

  // TODO - Dangerous-ish
  implicit object IntFormat extends Format[java.lang.Integer] {
    def writes(i: java.lang.Integer): JsValue =
      JsNumber(i.intValue)
    def reads(v: JsValue): JsResult[java.lang.Integer] = v match {
      case JsNumber(value) => JsSuccess(value.toInt)
      case _ => JsError("Unable to convert integer: " + v)
    }
  }
  // Generic Serializers
  implicit object UrlFormat extends Format[java.net.URL] {
    def writes(u: java.net.URL): JsValue =
      JsString(u.toURI.toASCIIString)
    def reads(v: JsValue): JsResult[java.net.URL] =
      v.validate[String].map(x => new java.net.URL(x))
  }
  implicit object FileFormat extends Format[java.io.File] {
    def writes(u: java.io.File): JsValue =
      JsString(u.toURI.toASCIIString)
    def reads(v: JsValue): JsResult[java.io.File] =
      v.validate[String].map(x => new java.io.File(new java.net.URI(x)))
  }
  implicit object UriFormat extends Format[java.net.URI] {
    def writes(u: java.net.URI): JsValue =
      JsString(u.toASCIIString)
    def reads(v: JsValue): JsResult[java.net.URI] =
      v.validate[String].map(x => new java.net.URI(x))
  }
  implicit def optionFormat[A](implicit other: Format[A]): Format[Option[A]] =
    new Format[Option[A]] {
      def writes(o: Option[A]): JsValue =
        o match {
          case Some(value) => other.writes(value)
          case None => JsNull // TODO - Is this ok?
        }
      def reads(v: JsValue): JsResult[Option[A]] =
        v match {
          case JsNull => JsSuccess(None)
          case value => other.reads(value).map(Some(_))
        }
    }
  // this is not implicit because it would cause trouble with
  // more specific formatters; we just use it as an explicit fallback
  object throwableFormat extends OFormat[java.lang.Throwable] {
    final private implicit def recursiveFormat = this
    def writes(t: java.lang.Throwable): JsObject =
      JsObject(Seq("message" -> Option(t.getMessage).map(JsString(_)).getOrElse(JsNull),
        "cause" -> Option(t.getCause).map(Json.toJson(_)).getOrElse(JsNull)))
    def reads(v: JsValue): JsResult[java.lang.Throwable] = {
      def validateOrNull[T <: AnyRef](json: JsValue)(implicit r: Reads[T]): JsResult[T] = json match {
        case JsNull => JsSuccess(null.asInstanceOf[T])
        case _ => r.reads(json)
      }
      for {
        message <- validateOrNull[String](v \ "message")
        cause <- validateOrNull[Throwable](v \ "cause")
      } yield new Exception(message, cause)
    }
  }

  implicit def attributedFormat[T](implicit format: Format[T]) =
    new Format[sbt.Attributed[T]] {
      override def writes(t: sbt.Attributed[T]): JsValue =
        JsObject(Seq("attributed" -> JsBoolean(true),
          "data" -> format.writes(t.data)))
      override def reads(obj: JsValue): JsResult[sbt.Attributed[T]] =
        (obj \ "attributed") match {
          case JsBoolean(true) =>
            format.reads(obj \ "data").map(sbt.Attributed.blank)
          case _ => JsError("not a valid attributed.")
        }
      override def toString = "Format[Attributed[" + format + "]"
    }
  implicit object SeverityFormat extends Format[xsbti.Severity] {
    override def writes(in: xsbti.Severity): JsValue =
      JsString(in.toString)
    override def reads(in: JsValue): JsResult[xsbti.Severity] =
      in match {
        case JsString(s) => Option(xsbti.Severity.valueOf(s)).map(sev => JsSuccess(sev)).getOrElse(JsError("Could not find severity: " + s))
        case _ => JsError("Could not find severity: " + in)
      }
  }

  implicit val xsbtiProblemFormat: Format[xsbti.Problem] =
    Format[xsbti.Problem](Reads(problemReads.reads),
      Writes(x => problemWrites.writes(Problem.fromXsbtiProblem(x))))

  private def convert[T](o: Option[T]): xsbti.Maybe[T] =
    o match {
      case Some(value) => xsbti.Maybe.just(value)
      case None => xsbti.Maybe.nothing()
    }

  def defineIf[T](value: xsbti.Maybe[T], name: String)(implicit format: Format[T]): Seq[(String, JsValue)] =
    if (value.isDefined) Seq(name -> format.writes(value.get)) else Nil

  private final case class PositionDeserialized(lineContent: String, l: Option[Int], o: Option[Int], p: Option[Int],
    ps: Option[String], sp: Option[String]) extends xsbti.Position {
    override def line = convert(l.map(Integer.valueOf))
    override def offset = convert(o.map(Integer.valueOf))
    override def pointer = convert(p.map(Integer.valueOf))
    override def pointerSpace = convert(ps)
    override def sourcePath = convert(sp)
    override def sourceFile = convert(sp.map(new java.io.File(_)))
  }

  private val positionReads: Reads[xsbti.Position] = (
    (__ \ "lineContent").read[String] and
    (__ \ "line").readNullable[Int] and
    (__ \ "offset").readNullable[Int] and
    (__ \ "pointer").readNullable[Int] and
    (__ \ "pointerSpace").readNullable[String] and
    (__ \ "sourcePath").readNullable[String])(PositionDeserialized.apply _)

  private val positionWrites: Writes[xsbti.Position] = Writes[xsbti.Position] { in =>
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

  implicit val positionFormat: Format[xsbti.Position] = Format[xsbti.Position](positionReads, positionWrites)

  private val executionAnalysisCommandFormat = Json.format[ExecutionAnalysisCommand]
  private val executionAnalysisKeyFormat = Json.format[ExecutionAnalysisKey]
  private val executionAnalysisErrorFormat = Json.format[ExecutionAnalysisError]

  private val executionAnalysisWrites: Writes[ExecutionAnalysis] = Writes[ExecutionAnalysis] { analysis =>
    val (discriminator, rest) =
      analysis match {
        case c: ExecutionAnalysisCommand => "command" -> executionAnalysisCommandFormat.writes(c)
        case k: ExecutionAnalysisKey => "key" -> executionAnalysisKeyFormat.writes(k)
        case e: ExecutionAnalysisError => "error" -> executionAnalysisErrorFormat.writes(e)
      }
    val baseObj = rest match {
      case o: JsObject => o
      case other => throw new RuntimeException(s"Serialized $analysis as a non-object $other")
    }
    baseObj ++ Json.obj("executionType" -> discriminator)
  }

  private val executionAnalysisReads: Reads[ExecutionAnalysis] = Reads[ExecutionAnalysis] { v =>
    (v \ "executionType").validate[String] flatMap {
      case "command" => executionAnalysisCommandFormat.reads(v)
      case "key" => executionAnalysisKeyFormat.reads(v)
      case "error" => executionAnalysisErrorFormat.reads(v)
      case other => JsError(s"Invalid executionType '$other' in $v")
    }
  }

  implicit val executionAnalysisFormat = Format[ExecutionAnalysis](executionAnalysisReads, executionAnalysisWrites)

  // Protocol serializers...  
  implicit val errorResponseFormat = Json.format[ErrorResponse]

  // EVENTS

  private implicit class FormatOps[T](val f: Format[T]) extends AnyVal {
    def withType(typ: String): Format[T] = {
      //usage of 'as' here is okay since we sort of assume that it will always be a JsObject
      val newWrites: Writes[T] = f.transform(x => x.as[JsObject] + ("type" -> JsString(typ)))
      Format(f, newWrites)
    }
  }

  private val logSuccessFormat: Format[LogSuccess] = Json.format[LogSuccess].withType("success")
  private val logMessageFormat: Format[LogMessage] = Json.format[LogMessage].withType("message")
  private val logStdOutFormat: Format[LogStdOut] = Json.format[LogStdOut].withType("stdout")
  private val logStdErrFormat: Format[LogStdErr] = Json.format[LogStdErr].withType("stderr")

  private val logTraceReads = (
    (__ \ "class").read[String] and
    (__ \ "message").read[String])(LogTrace.apply _)

  private val logTraceWrites = (
    (__ \ "class").write[String] and
    (__ \ "message").write[String])(unlift(LogTrace.unapply))

  private implicit val logTraceFormat = Format(logTraceReads, logTraceWrites).withType("trace")

  //play-json does not handle polymorphic writes and reads very well at all
  private val logEntryWrites: Writes[LogEntry] = Writes[LogEntry] {
    case x: LogSuccess => logSuccessFormat.writes(x)
    case x: LogTrace => logTraceWrites.writes(x)
    case x: LogMessage => logMessageFormat.writes(x)
    case x: LogStdOut => logStdOutFormat.writes(x)
    case x: LogStdErr => logStdErrFormat.writes(x)
  }

  private val logEntryReads: Reads[LogEntry] = Reads[LogEntry] { js =>
    (js \ "type").validate[String].flatMap {
      case "success" => logSuccessFormat.reads(js)
      case "trace" => logTraceReads.reads(js)
      case "message" => logMessageFormat.reads(js)
      case "stdout" => logStdOutFormat.reads(js)
      case "stderr" => logStdErrFormat.reads(js)
      case other => JsError(s"Unknown log entry type $other")
    }
  }
  implicit val logEntryFormat: Format[LogEntry] = Format(logEntryReads, logEntryWrites)

  private def emptyObjectFormat[A](instance: A) = new Format[A] {
    def writes(e: A): JsValue = JsObject(Seq.empty)
    def reads(obj: JsValue): JsResult[A] = JsSuccess(instance)
  }
  implicit val receivedResponseFormat = emptyObjectFormat(ReceivedResponse())

  implicit val requestCompletedFormat = emptyObjectFormat(RequestCompleted())
  implicit val requestFailedFormat = emptyObjectFormat(RequestFailed())
  implicit val killRequestFormat = emptyObjectFormat(KillServerRequest())

  implicit val outcomeFormat: Format[TestOutcome] = Format(
    Reads[TestOutcome] { value =>
      value.validate[String].flatMap {
        case "passed" => JsSuccess(TestPassed)
        case "failed" => JsSuccess(TestFailed)
        case "error" => JsSuccess(TestError)
        case "skipped" => JsSuccess(TestSkipped)
        case other => JsError(s"Unknown test outcome - $other")
      }
    },
    Writes[TestOutcome](outcome => JsString(outcome.toString)))
  implicit val testGroupResultFormat: Format[TestGroupResult] = Format(
    Reads[TestGroupResult] { value =>
      value.validate[String].flatMap {
        case "passed" => JsSuccess(TestGroupPassed)
        case "failed" => JsSuccess(TestGroupFailed)
        case "error" => JsSuccess(TestGroupError)
        case other => JsError(s"Unknown test group result - $other")
      }
    },
    Writes[TestGroupResult](result => JsString(result.toString)))

  implicit val taskLogEventFormat = Json.format[TaskLogEvent]
  implicit val coreLogEventFormat = Json.format[CoreLogEvent]
  implicit val backgroundJobLogEventFormat = Json.format[BackgroundJobLogEvent]
  implicit val cancelExecutionRequestFormat = Json.format[CancelExecutionRequest]
  implicit val cancelExecutionResponseFormat = Json.format[CancelExecutionResponse]
  implicit val clientInfoFormat = Json.format[ClientInfo]
  implicit val registerClientRequestFormat = Json.format[RegisterClientRequest]
  implicit val executionRequestFormat = Json.format[ExecutionRequest]
  implicit val keyExecutionRequestFormat = Json.format[KeyExecutionRequest]
  implicit val executionReceivedFormat = Json.format[ExecutionRequestReceived]
  implicit val executionWaitingFormat = Json.format[ExecutionWaiting]
  implicit val executionStartingFormat = Json.format[ExecutionStarting]
  implicit val executionSuccessFormat = Json.format[ExecutionSuccess]
  implicit val executionFailureFormat = Json.format[ExecutionFailure]
  implicit val listenToEventsFormat = emptyObjectFormat(ListenToEvents())
  implicit val unlistenToEventsFormat = emptyObjectFormat(UnlistenToEvents())
  implicit val listenToBuildChangeFormat = emptyObjectFormat(ListenToBuildChange())
  implicit val unlistenToBuildChangeFormat = emptyObjectFormat(UnlistenToBuildChange())
  implicit val sendSyntheticBuildChangedFormat = emptyObjectFormat(SendSyntheticBuildChanged())
  implicit val buildStructureChangedFormat = Json.format[BuildStructureChanged]
  implicit val listenToValueFormat = Json.format[ListenToValue]
  implicit val unlistenToValueFormat = Json.format[UnlistenToValue]
  implicit val sendSyntheticValueChangedFormat = Json.format[SendSyntheticValueChanged]
  implicit val keyNotFoundFormat = Json.format[KeyNotFound]
  implicit val taskStartedFormat = Json.format[TaskStarted]
  implicit val taskFinishedFormat = Json.format[TaskFinished]
  implicit val readLineRequestFormat = Json.format[ReadLineRequest]
  implicit val readLineResponseFormat = Json.format[ReadLineResponse]
  implicit val confirmRequestFormat = Json.format[ConfirmRequest]
  implicit val confirmResponseFormat = Json.format[ConfirmResponse]
  implicit val keyLookupRequestFormat = Json.format[KeyLookupRequest]
  implicit val keyLookupResponseFormat = Json.format[KeyLookupResponse]
  implicit val analyzeExecutionRequestFormat = Json.format[AnalyzeExecutionRequest]
  implicit val analyzeExecutionResponseFormat = Json.format[AnalyzeExecutionResponse]
  implicit val buildLoadedFormat = emptyObjectFormat(BuildLoaded())
  implicit val buildFailedToLoadFormat = emptyObjectFormat(BuildFailedToLoad())
  implicit val backgroundJobInfoFormat = Json.format[BackgroundJobInfo]
  implicit val backgroundJobStartedFormat = Json.format[BackgroundJobStarted]
  implicit val backgroundJobFinishedFormat = Json.format[BackgroundJobFinished]

  // This needs a custom formatter because it has a custom apply/unapply
  // which confuses the auto-formatter macro
  private val taskEventWrites: Writes[TaskEvent] = (
    (__ \ "taskId").write[Long] and
    (__ \ "name").write[String] and
    (__ \ "serialized").write[JsValue])(unlift(TaskEvent.unapply))

  private val taskEventReads: Reads[TaskEvent] = (
    (__ \ "taskId").read[Long] and
    (__ \ "name").read[String] and
    (__ \ "serialized").read[JsValue])((id, name, serialized) => TaskEvent(id, name, serialized))

  implicit val taskEventFormat: Format[TaskEvent] = Format[TaskEvent](taskEventReads, taskEventWrites)

  implicit def valueChangedReads[A, E <: Throwable](implicit result: Reads[TaskResult[A, E]]): Reads[ValueChanged[A, E]] = (
    (__ \ "key").read[ScopedKey] and
    (__ \ "value").read[TaskResult[A, E]])(ValueChanged.apply[A, E] _)

  implicit def valueChangedWrites[A, E <: Throwable](implicit result: Writes[TaskResult[A, E]]): Writes[ValueChanged[A, E]] = (
    (__ \ "key").write[ScopedKey] and
    (__ \ "value").write[TaskResult[A, E]])(unlift(ValueChanged.unapply[A, E]))

  implicit def valueChangedFormat[A, E <: Throwable](implicit result: Format[TaskResult[A, E]]): Format[ValueChanged[A, E]] =
    Format(valueChangedReads, valueChangedWrites)

  // This needs a custom formatter because it has a custom apply/unapply
  // which confuses the auto-formatter macro
  implicit val backgroundJobEventFormat: Format[BackgroundJobEvent] = new Format[BackgroundJobEvent] {
    override def writes(event: BackgroundJobEvent): JsValue = {
      Json.obj("jobId" -> event.jobId, "name" -> event.name, "serialized" -> event.serialized)
    }

    override def reads(v: JsValue): JsResult[BackgroundJobEvent] = {
      for {
        jobId <- (v \ "jobId").validate[Long]
        name <- (v \ "name").validate[String]
        serialized = (v \ "serialized")
      } yield BackgroundJobEvent(jobId = jobId, name = name, serialized = serialized)
    }
  }

  implicit val completionFormat = Json.format[Completion]
  implicit val commandCompletionsRequestFormat = Json.format[CommandCompletionsRequest]
  implicit val commandCompletionsResponseFormat = Json.format[CommandCompletionsResponse]

  ///// task events (do not extend protocol.Message)
  // these formatters are hand-coded because they have an unapply()
  // that confuses play-json

  private val testGroupStartedReads: Reads[TestGroupStarted] =
    (__ \ "name").read[String].map(TestGroupStarted(_))
  private val testGroupStartedWrites: Writes[TestGroupStarted] =
    (__ \ "name").write[String].contramap(x => x.name)

  implicit val testGroupFormat: Format[TestGroupStarted] = Format(testGroupStartedReads, testGroupStartedWrites)

  private val testGroupFinishedReads: Reads[TestGroupFinished] = (
    (__ \ "name").read[String] and
    (__ \ "result").read[TestGroupResult] and
    (__ \ "error").readNullable[String])(TestGroupFinished.apply _)
  private val testGroupFinishedWrites: Writes[TestGroupFinished] = (
    (__ \ "name").write[String] and
    (__ \ "result").write[TestGroupResult] and
    (__ \ "error").writeNullable[String])(unlift(TestGroupFinished.unapply))
  implicit val testGroupFinishedFormat: Format[TestGroupFinished] = Format(testGroupFinishedReads, testGroupFinishedWrites)

  private val testEventReads: Reads[TestEvent] = (
    (__ \ "name").read[String] and
    (__ \ "description").readNullable[String] and
    (__ \ "outcome").read[TestOutcome] and
    (__ \ "error").readNullable[String] and
    (__ \ "duration").read[Long])(TestEvent.apply _)

  private val testEventWrites: Writes[TestEvent] = Writes[TestEvent] { event =>
    Json.obj("name" -> event.name, "description" -> event.description,
      "outcome" -> event.outcome, "error" -> event.error, "duration" -> event.duration)
  }
  implicit val testEventFormat: Format[TestEvent] = Format[TestEvent](testEventReads, testEventWrites)

  private val compilationFailureReads: Reads[CompilationFailure] = (
    (__ \ "project").read[ProjectReference] and
    (__ \ "position").read[xsbti.Position] and
    (__ \ "severity").read[xsbti.Severity] and
    (__ \ "message").read[String])(CompilationFailure.apply _)

  private val compilationFailureWrites: Writes[CompilationFailure] = Writes[CompilationFailure] { event =>
    Json.obj("project" -> event.project, "position" -> event.position,
      "severity" -> event.severity, "message" -> event.message)
  }

  implicit val compilationFailureFormat = Format[CompilationFailure](compilationFailureReads, compilationFailureWrites)

  implicit val mutableByteArrayReads: Reads[Array[Byte]] = Reads(_.asOpt[String] map { in =>
    try {
      val r = hexToBytes(in)
      JsSuccess(r)
    } catch {
      case e: Exception => JsError(Seq(JsPath() -> Seq(ValidationError(s"Could not decode string '$in' as hex."))))
    }
  } getOrElse JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected.jsstring")))))
  implicit val mutableByteArrayWrites: Writes[Array[Byte]] = Writes(in => JsString(bytesToHex(in)))
  implicit val mutableByteArrayFormat: Format[Array[Byte]] = Format[Array[Byte]](mutableByteArrayReads, mutableByteArrayWrites)
  implicit val immutableByteArrayReads: Reads[ByteArray] = Reads(_.asOpt[String] map { in =>
    try {
      val r = hexToBytes(in)
      JsSuccess(ByteArray(r))
    } catch {
      case e: Exception => JsError(Seq(JsPath() -> Seq(ValidationError(s"Could not decode string '$in' as hex."))))
    }
  } getOrElse JsError(Seq(JsPath() -> Seq(ValidationError("validate.error.expected.jsstring")))))
  implicit val immutableByteArrayWrites: Writes[ByteArray] = Writes(in => JsString(bytesToHex(in.toArray)))
  implicit val immutableByteArrayFormat: Format[ByteArray] = Format[ByteArray](immutableByteArrayReads, immutableByteArrayWrites)
  implicit def setReads[A](implicit aReads: Reads[A]): Reads[Set[A]] = new Reads[Set[A]] {
    def reads(json: JsValue) = json.validate[Seq[A]].map(_.toSet)
  }
  implicit def setWrites[A](implicit aWrites: Writes[A]): Writes[Set[A]] = new Writes[Set[A]] {
    def writes(in: Set[A]) = JsArray(in.map(aWrites.writes).toSeq)
  }
  implicit def setFormat[A](implicit aFormat: Format[A]): Format[Set[A]] = Format[Set[A]](setReads, setWrites)
  implicit def mapReads[A, B](implicit aReads: Reads[A], bReads: Reads[B]): Reads[Map[A, B]] = new Reads[Map[A, B]] {
    def reads(json: JsValue) = json match {
      case JsArray(in) =>
        in.foldLeft[JsResult[Map[A, B]]](JsSuccess(Map.empty[A, B])) { (s, v) =>
          (s, v) match {
            case (JsSuccess(m, _), JsArray(v)) =>
              if (v.size == 2) {
                (Json.fromJson[A](v(0)), Json.fromJson[B](v(1))) match {
                  case (JsSuccess(a, _), JsSuccess(b, _)) => JsSuccess(m + (a -> b))
                  case (ea @ JsError(_), JsSuccess(_, _)) => ea
                  case (JsSuccess(_, _), eb @ JsError(_)) => eb
                  case (ea @ JsError(_), eb @ JsError(_)) => ea ++ eb
                }
              } else JsError(Seq(JsPath() -> Seq(ValidationError("expected 2 elements in map entry, got: ${v.size} [$v]"))))
            case (JsSuccess(_, _), v) => JsError(Seq(JsPath() -> Seq(ValidationError("expected 2 elements in map entry, got: $v"))))
            case (x @ JsError(_), _) => x
          }
        }
      case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected an array of two elements. Got: $x"))))
    }
  }
  implicit def mapWrites[A, B](implicit aWrites: Writes[A], bWrites: Writes[B]): Writes[Map[A, B]] = new Writes[Map[A, B]] {
    def writes(in: Map[A, B]) = JsArray(in.foldLeft(Seq.empty[JsArray]) {
      case (s, (k, v)) =>
        val kj = aWrites.writes(k)
        val vj = bWrites.writes(v)
        s :+ Json.arr(kj, vj).asInstanceOf[JsArray]
    })
  }
  implicit def mapFormat[A, B](implicit aFormat: Format[A], bFormat: Format[B]): Format[Map[A, B]] = Format[Map[A, B]](mapReads, mapWrites)
  implicit def relationReads[A, B](implicit aReads: Reads[A], bReads: Reads[B]): Reads[Relation[A, B]] = new Reads[Relation[A, B]] {
    def reads(json: JsValue) =
      ((__ \ "forwardMap").read[Map[A, Set[B]]] and
        (__ \ "reverseMap").read[Map[B, Set[A]]]).apply(Relation(_, _)).reads(json)
  }
  implicit def relationWrites[A, B](implicit aWrites: Writes[A], bWrites: Writes[B]): Writes[Relation[A, B]] = new Writes[Relation[A, B]] {
    def writes(in: Relation[A, B]) = Json.obj("forwardMap" -> in.forwardMap, "reverseMap" -> in.reverseMap)
  }
  implicit def relationFormat[A, B](implicit aFormat: Format[A], bFormat: Format[B], mfa: Manifest[A], mfb: Manifest[B]): Format[Relation[A, B]] = Format[Relation[A, B]](relationReads, relationWrites)
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
  implicit val stampFormat: Format[Stamp] = Format[Stamp](stampReads, stampWrites)
  implicit val stampsFormat: Format[Stamps] = Json.format[Stamps]
  implicit val problemReads: Reads[Problem] = Json.reads[Problem]
  // this one causes ambiguity trouble with Writes[xsbti.Problem] and isn't needed
  // implicitly since the xsbti.Problem writes is just fine.
  private val problemWrites: Writes[Problem] = Json.writes[Problem]

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
  implicit val qualifierFormat: Format[Qualifier] = Format[Qualifier](qualifierReads, qualifierWrites)
  implicit val accessFormat: Format[Access] = new Format[Access] {
    def writes(o: Access): JsValue = o match {
      case Public => emitTypedValue("public")
      case x: Protected => emitTypedValue("protected", "qualifier" -> x.qualifier)
      case x: Private => emitTypedValue("private", "qualifier" -> x.qualifier)
    }
    def reads(json: JsValue): JsResult[Access] =
      (json \ "type").validate[String].flatMap {
        _ match {
          case "public" => JsSuccess(Public)
          case "protected" => (json \ "qualifier").validate[Qualifier].map(Protected(_))
          case "private" => (json \ "qualifier").validate[Qualifier].map(Private(_))
          case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'public', 'protected', or 'private', got: $x"))))
        }
      }
  }
  implicit val varianceFormat: Format[xsbti.api.Variance] = new Format[xsbti.api.Variance] {
    import xsbti.api.Variance._
    def writes(o: xsbti.api.Variance): JsValue = o match {
      case Invariant => emitTypedValue("invariant")
      case Covariant => emitTypedValue("covariant")
      case Contravariant => emitTypedValue("contravariant")
    }
    def reads(json: JsValue): JsResult[xsbti.api.Variance] =
      (json \ "type").validate[String].flatMap {
        _ match {
          case "invariant" => JsSuccess(Invariant)
          case "covariant" => JsSuccess(Covariant)
          case "contravariant" => JsSuccess(Contravariant)
          case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'invariant', 'covariant', or 'contravariant', got: $x"))))
        }
      }
  }
  implicit val pathComponentFormat: Format[PathComponent] = new Format[PathComponent] {
    def writes(o: PathComponent): JsValue = o match {
      case x: Id => emitTypedValue("id", "id" -> x.id)
      case x: Super => emitTypedValue("super", "qualifier" -> x.qualifier)
      case This => emitTypedValue("this")
    }
    def reads(json: JsValue): JsResult[PathComponent] =
      (json \ "type").validate[String].flatMap {
        _ match {
          case "id" => (json \ "path").validate[String].map(Id(_))
          case "super" => (json \ "qualifier").validate[Path].map(Super(_))
          case "this" => JsSuccess(This)
          case x => JsError(Seq(JsPath() -> Seq(ValidationError(s"Expected 'id', 'super', or 'this', got: $x"))))
        }
      }
  }
  implicit val pathFormat: Format[Path] = Json.format[Path]
  implicit val typeParameterFormat: Format[TypeParameter] = Json.format[TypeParameter]

  implicit val simpleTypeReads: Reads[SimpleType] = new Reads[SimpleType] {
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
  private val simpleTypeWrites: Writes[SimpleType] = new Writes[SimpleType] {
    def writes(o: SimpleType): JsValue = o match {
      case x: Singleton => emitTypedValue("singleton", "path" -> x.path)
      case x: Projection => emitTypedValue("projection", "prefix" -> writes(x.prefix), "id" -> x.id)
      case x: Parameterized => emitTypedValue("parameterized", "baseType" -> writes(x.baseType), "typeArguments" -> x.typeArguments)
      case x: ParameterRef => emitTypedValue("parameterRef", "id" -> x.id)
      case EmptyType => emitTypedValue("emptyType")
    }
  }
  implicit val typeReads: Reads[Type] = new Reads[Type] {
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
  implicit val typeWrites: Writes[Type] = new Writes[Type] {
    def writes(o: Type): JsValue = o match {
      case x: SimpleType => simpleTypeWrites.writes(x)
      case x: Annotated => emitTypedValue("annotated", "baseType" -> x.baseType, "annotations" -> x.annotations)
      case x: Structure => emitTypedValue("structure", "parents" -> x.parents, "declared" -> x.declared, "inherited" -> x.inherited)
      case x: Polymorphic => emitTypedValue("polymorphic", "baseType" -> x.baseType, "parameters" -> x.parameters)
      case x: Existential => emitTypedValue("existential", "baseType" -> x.baseType, "clause" -> x.clause)
      case x: Constant => emitTypedValue("constant", "baseType" -> x.baseType, "value" -> x.value)
    }
  }
  implicit val typeFormat: Format[Type] = Format[Type](typeReads, typeWrites)

  implicit val annotationArgumentFormat: Format[AnnotationArgument] = Json.format[AnnotationArgument]
  implicit val annotationFormat: Format[Annotation] = Json.format[Annotation]
  implicit val packageFormat: Format[Package] = Json.format[Package]
  implicit val sourceInfoFormat: Format[SourceInfo] = Json.format[SourceInfo]
  implicit val sourceInfosFormat: Format[SourceInfos] = Json.format[SourceInfos]
  implicit val outputSettingFormat: Format[OutputSetting] = Json.format[OutputSetting]
  implicit val modifiersFormat: Format[Modifiers] = Json.format[Modifiers]
  implicit val definitionFormat: Format[Definition] = Json.format[Definition]
  implicit val compilationFormat: Format[Compilation] = Json.format[Compilation]
  implicit val sourceAPIFormat: Format[SourceAPI] = Json.format[SourceAPI]
  implicit val sourceFormat: Format[Source] = Json.format[Source]
  implicit val apisFormat: Format[APIs] = Json.format[APIs]
  implicit val compilationsFormat: Format[Compilations] = Json.format[Compilations]
  implicit val relationsSourceFormat: Format[RelationsSource] = Json.format[RelationsSource]
  implicit val relationsFormat: Format[Relations] = Json.format[Relations]
  implicit val analysisFormat: Format[Analysis] = Json.format[Analysis]

  implicit val compileFailedExceptionReads: Reads[CompileFailedException] = new Reads[CompileFailedException] {
    override def reads(json: JsValue): JsResult[CompileFailedException] = {
      for {
        t <- throwableFormat.reads(json)
        problems <- (json \ "problems").validate[Seq[xsbti.Problem]]
      } yield new CompileFailedException(t.getMessage, t.getCause, problems)
    }
  }

  implicit val compileFailedExceptionWrites: Writes[CompileFailedException] = new Writes[CompileFailedException] {
    override def writes(e: CompileFailedException): JsValue = {
      val json = throwableFormat.writes(e)
      json ++ Json.obj("problems" -> e.problems)
    }
  }

  implicit val compileFailedExceptionFormat: Format[CompileFailedException] =
    Format(compileFailedExceptionReads, compileFailedExceptionWrites)
}
