package sbt.protocol

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import JsonHelpers._

private[protocol] object MessageSerialization {

  private val executionAnalysisCommandReads = Json.reads[ExecutionAnalysisCommand]
  private val executionAnalysisCommandWrites = Json.writes[ExecutionAnalysisCommand]
  private val executionAnalysisKeyReads = Json.reads[ExecutionAnalysisKey]
  private val executionAnalysisKeyWrites = Json.writes[ExecutionAnalysisKey]
  private val executionAnalysisErrorReads = Json.reads[ExecutionAnalysisError]
  private val executionAnalysisErrorWrites = Json.writes[ExecutionAnalysisError]

  implicit val executionAnalysisWrites: Writes[ExecutionAnalysis] = Writes[ExecutionAnalysis] { analysis =>
    val (discriminator, rest) =
      analysis match {
        case c: ExecutionAnalysisCommand => "command" -> executionAnalysisCommandWrites.writes(c)
        case k: ExecutionAnalysisKey => "key" -> executionAnalysisKeyWrites.writes(k)
        case e: ExecutionAnalysisError => "error" -> executionAnalysisErrorWrites.writes(e)
      }
    val baseObj = rest match {
      case o: JsObject => o
      case other => throw new RuntimeException(s"Serialized $analysis as a non-object $other")
    }
    baseObj ++ Json.obj("executionType" -> discriminator)
  }

  implicit val executionAnalysisReads: Reads[ExecutionAnalysis] = Reads[ExecutionAnalysis] { v =>
    (v \ "executionType").validate[String] flatMap {
      case "command" => executionAnalysisCommandReads.reads(v)
      case "key" => executionAnalysisKeyReads.reads(v)
      case "error" => executionAnalysisErrorReads.reads(v)
      case other => JsError(s"Invalid executionType '$other' in $v")
    }
  }

  // Protocol serializers...
  implicit val errorResponseReads = Json.reads[ErrorResponse]
  implicit val errorResponseWrites = Json.writes[ErrorResponse]

  // EVENTS

  private implicit class OWritesOps[T](val w: OWrites[T]) extends AnyVal {
    def withType(typ: String): OWrites[T] = {
      OWrites(t => w.writes(t) + ("type" -> JsString(typ)))
    }
  }

  private val logSuccessReads = Json.reads[LogSuccess]
  private val logSuccessWrites = Json.writes[LogSuccess].withType("success")
  private val logMessageReads = Json.reads[LogMessage]
  private val logMessageWrites = Json.writes[LogMessage].withType("message")
  private val logStdOutReads = Json.reads[LogStdOut]
  private val logStdOutWrites = Json.writes[LogStdOut].withType("stdout")
  private val logStdErrReads = Json.reads[LogStdErr]
  private val logStdErrWrites = Json.writes[LogStdErr].withType("stderr")

  private val logTraceReads = (
    (__ \ "class").read[String] and
    (__ \ "message").read[String])(LogTrace.apply _)

  private val logTraceWrites = (
    (__ \ "class").write[String] and
    (__ \ "message").write[String])(unlift(LogTrace.unapply)).withType("trace")

  //play-json does not handle polymorphic writes and reads very well at all
  private implicit val logEntryWrites: Writes[LogEntry] = Writes[LogEntry] {
    case x: LogSuccess => logSuccessWrites.writes(x)
    case x: LogTrace => logTraceWrites.writes(x)
    case x: LogMessage => logMessageWrites.writes(x)
    case x: LogStdOut => logStdOutWrites.writes(x)
    case x: LogStdErr => logStdErrWrites.writes(x)
  }

  private implicit val logEntryReads: Reads[LogEntry] = Reads[LogEntry] { js =>
    (js \ "type").validate[String].flatMap {
      case "success" => logSuccessReads.reads(js)
      case "trace" => logTraceReads.reads(js)
      case "message" => logMessageReads.reads(js)
      case "stdout" => logStdOutReads.reads(js)
      case "stderr" => logStdErrReads.reads(js)
      case other => JsError(s"Unknown log entry type $other")
    }
  }

  private def emptyObjectReads[A](instance: A) = Reads[A](_ => JsSuccess(instance))
  private def emptyObjectWrites[A] = Writes[A](_ => JsObject(Nil))

  implicit val receivedResponseReads = emptyObjectReads(ReceivedResponse())
  implicit val receivedResponseWrites = emptyObjectWrites[ReceivedResponse]

  implicit val killRequestReads = emptyObjectReads(KillServerRequest())
  implicit val killRequestWrites = emptyObjectWrites[KillServerRequest]

  implicit val taskLogEventReads = Json.reads[TaskLogEvent]
  implicit val taskLogEventWrites = Json.writes[TaskLogEvent]
  implicit val coreLogEventReads = Json.reads[CoreLogEvent]
  implicit val coreLogEventWrites = Json.writes[CoreLogEvent]
  implicit val backgroundJobLogEventReads = Json.reads[BackgroundJobLogEvent]
  implicit val backgroundJobLogEventWrites = Json.writes[BackgroundJobLogEvent]
  implicit val cancelExecutionRequestReads = Json.reads[CancelExecutionRequest]
  implicit val cancelExecutionRequestWrites = Json.writes[CancelExecutionRequest]
  implicit val cancelExecutionResponseReads = Json.reads[CancelExecutionResponse]
  implicit val cancelExecutionResponseWrites = Json.writes[CancelExecutionResponse]
  implicit val clientInfoReads = Json.reads[ClientInfo]
  implicit val clientInfoWrites = Json.writes[ClientInfo]
  implicit val registerClientRequestReads = Json.reads[RegisterClientRequest]
  implicit val registerClientRequestWrites = Json.writes[RegisterClientRequest]
  implicit val executionRequestReads = Json.reads[ExecutionRequest]
  implicit val executionRequestWrites = Json.writes[ExecutionRequest]
  implicit val keyExecutionRequestReads = Json.reads[KeyExecutionRequest]
  implicit val keyExecutionRequestWrites = Json.writes[KeyExecutionRequest]
  implicit val executionReceivedReads = Json.reads[ExecutionRequestReceived]
  implicit val executionReceivedWrites = Json.writes[ExecutionRequestReceived]
  implicit val executionWaitingReads = Json.reads[ExecutionWaiting]
  implicit val executionWaitingWrites = Json.writes[ExecutionWaiting]
  implicit val executionStartingReads = Json.reads[ExecutionStarting]
  implicit val executionStartingWrites = Json.writes[ExecutionStarting]
  implicit val executionSuccessReads = Json.reads[ExecutionSuccess]
  implicit val executionSuccessWrites = Json.writes[ExecutionSuccess]
  implicit val executionFailureReads = Json.reads[ExecutionFailure]
  implicit val executionFailureWrites = Json.writes[ExecutionFailure]
  implicit val listenToEventsReads = emptyObjectReads(ListenToEvents())
  implicit val listenToEventsWrites = emptyObjectWrites[ListenToEvents]
  implicit val unlistenToEventsReads = emptyObjectReads(UnlistenToEvents())
  implicit val unlistenToEventsWrites = emptyObjectWrites[UnlistenToEvents]
  implicit val listenToBuildChangeReads = emptyObjectReads(ListenToBuildChange())
  implicit val listenToBuildChangeWrites = emptyObjectWrites[ListenToBuildChange]
  implicit val unlistenToBuildChangeReads = emptyObjectReads(UnlistenToBuildChange())
  implicit val unlistenToBuildChangeWrites = emptyObjectWrites[UnlistenToBuildChange]
  implicit val sendSyntheticBuildChangedReads = emptyObjectReads(SendSyntheticBuildChanged())
  implicit val sendSyntheticBuildChangedWrites = emptyObjectWrites[SendSyntheticBuildChanged]
  implicit val buildStructureChangedReads = Json.reads[BuildStructureChanged]
  implicit val buildStructureChangedWrites = Json.writes[BuildStructureChanged]
  implicit val listenToValueReads = Json.reads[ListenToValue]
  implicit val listenToValueWrites = Json.writes[ListenToValue]
  implicit val unlistenToValueReads = Json.reads[UnlistenToValue]
  implicit val unlistenToValueWrites = Json.writes[UnlistenToValue]
  implicit val sendSyntheticValueChangedReads = Json.reads[SendSyntheticValueChanged]
  implicit val sendSyntheticValueChangedWrites = Json.writes[SendSyntheticValueChanged]
  implicit val keyNotFoundReads = Json.reads[KeyNotFound]
  implicit val keyNotFoundWrites = Json.writes[KeyNotFound]
  implicit val taskStartedReads = Json.reads[TaskStarted]
  implicit val taskStartedWrites = Json.writes[TaskStarted]
  implicit val taskFinishedReads = Json.reads[TaskFinished]
  implicit val taskFinishedWrites = Json.writes[TaskFinished]
  implicit val readLineRequestReads = Json.reads[ReadLineRequest]
  implicit val readLineRequestWrites = Json.writes[ReadLineRequest]
  implicit val readLineResponseReads = Json.reads[ReadLineResponse]
  implicit val readLineResponseWrites = Json.writes[ReadLineResponse]
  implicit val confirmRequestReads = Json.reads[ConfirmRequest]
  implicit val confirmRequestWrites = Json.writes[ConfirmRequest]
  implicit val confirmResponseReads = Json.reads[ConfirmResponse]
  implicit val confirmResponseWrites = Json.writes[ConfirmResponse]
  implicit val keyLookupRequestReads = Json.reads[KeyLookupRequest]
  implicit val keyLookupRequestWrites = Json.writes[KeyLookupRequest]
  implicit val keyLookupResponseReads = Json.reads[KeyLookupResponse]
  implicit val keyLookupResponseWrites = Json.writes[KeyLookupResponse]
  implicit val analyzeExecutionRequestReads = Json.reads[AnalyzeExecutionRequest]
  implicit val analyzeExecutionRequestWrites = Json.writes[AnalyzeExecutionRequest]
  implicit val analyzeExecutionResponseReads = Json.reads[AnalyzeExecutionResponse]
  implicit val analyzeExecutionResponseWrites = Json.writes[AnalyzeExecutionResponse]
  implicit val buildLoadedReads = emptyObjectReads(BuildLoaded())
  implicit val buildLoadedWrites = emptyObjectWrites[BuildLoaded]
  implicit val buildFailedToLoadReads = emptyObjectReads(BuildFailedToLoad())
  implicit val buildFailedToLoadWrites = emptyObjectWrites[BuildFailedToLoad]
  implicit val backgroundJobInfoReads = Json.reads[BackgroundJobInfo]
  implicit val backgroundJobInfoWrites = Json.writes[BackgroundJobInfo]
  implicit val backgroundJobStartedReads = Json.reads[BackgroundJobStarted]
  implicit val backgroundJobStartedWrites = Json.writes[BackgroundJobStarted]
  implicit val backgroundJobFinishedReads = Json.reads[BackgroundJobFinished]
  implicit val backgroundJobFinishedWrites = Json.writes[BackgroundJobFinished]

  // This needs a custom formatter because it has a custom apply/unapply
  // which confuses the auto-formatter macro
  implicit val taskEventWrites: Writes[TaskEvent] = (
    (__ \ "taskId").write[Long] and
    (__ \ "name").write[String] and
    (__ \ "serialized").write[JsValue])(unlift(TaskEvent.unapply))

  implicit val taskEventReads: Reads[TaskEvent] = (
    (__ \ "taskId").read[Long] and
    (__ \ "name").read[String] and
    (__ \ "serialized").read[JsValue])((id, name, serialized) => TaskEvent(id, name, serialized))

  implicit val valueChangedReads = Json.reads[ValueChanged]

  implicit val valueChangedWrites = Json.writes[ValueChanged]

  // This needs a custom formatter because it has a custom apply/unapply
  // which confuses the auto-formatter macro
  implicit val backgroundJobEventWrites = Writes[BackgroundJobEvent] { event =>
    Json.obj("jobId" -> event.jobId, "name" -> event.name, "serialized" -> event.serialized)
  }

  implicit val backgroundJobEventReads = Reads[BackgroundJobEvent] { v =>
    for {
      jobId <- (v \ "jobId").validate[Long]
      name <- (v \ "name").validate[String]
      serialized = (v \ "serialized")
    } yield BackgroundJobEvent(jobId = jobId, name = name, serialized = serialized)
  }

  implicit val completionReads = Json.reads[Completion]
  implicit val completionWrites = Json.writes[Completion]
  implicit val commandCompletionsRequestReads = Json.reads[CommandCompletionsRequest]
  implicit val commandCompletionsRequestWrites = Json.writes[CommandCompletionsRequest]
  implicit val commandCompletionsResponseReads = Json.reads[CommandCompletionsResponse]
  implicit val commandCompletionsResponseWrites = Json.writes[CommandCompletionsResponse]
}