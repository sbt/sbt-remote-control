package sbt

import play.api.libs.json._
import play.api.libs.functional.syntax._

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

  implicit def valueChangedReads[A](implicit result: Reads[TaskResult[A]]): Reads[ValueChanged[A]] = (
    (__ \ "key").read[ScopedKey] and
    (__ \ "value").read[TaskResult[A]])(ValueChanged.apply[A] _)

  implicit def valueChangedWrites[A](implicit result: Writes[TaskResult[A]]): Writes[ValueChanged[A]] = (
    (__ \ "key").write[ScopedKey] and
    (__ \ "value").write[TaskResult[A]])(unlift(ValueChanged.unapply[A]))

  implicit def valueChangedFormat[A](implicit result: Format[TaskResult[A]]): Format[ValueChanged[A]] = Format(valueChangedReads, valueChangedWrites)

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
}
