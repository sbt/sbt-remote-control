package sbt.protocol

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import JsonHelpers._

/** The implementation of Message serialization */
private[protocol] object MessageSerialization {

  private val executionAnalysisCommandReads = Json.reads[ExecutionAnalysisCommand]
  private val executionAnalysisCommandWrites = Json.writes[ExecutionAnalysisCommand]
  private val executionAnalysisKeyReads = Json.reads[ExecutionAnalysisKey]
  private val executionAnalysisKeyWrites = Json.writes[ExecutionAnalysisKey]
  private val executionAnalysisErrorReads = Json.reads[ExecutionAnalysisError]
  private val executionAnalysisErrorWrites = Json.writes[ExecutionAnalysisError]

  private implicit val executionAnalysisWrites: Writes[ExecutionAnalysis] = Writes[ExecutionAnalysis] { analysis =>
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

  private implicit val executionAnalysisReads: Reads[ExecutionAnalysis] = Reads[ExecutionAnalysis] { v =>
    (v \ "executionType").validate[String] flatMap {
      case "command" => executionAnalysisCommandReads.reads(v)
      case "key" => executionAnalysisKeyReads.reads(v)
      case "error" => executionAnalysisErrorReads.reads(v)
      case other => JsError(s"Invalid executionType '$other' in $v")
    }
  }

  // Protocol serializers...
  private implicit val errorResponseReads = Json.reads[ErrorResponse]
  private implicit val errorResponseWrites = Json.writes[ErrorResponse]

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

  private implicit val receivedResponseReads = emptyObjectReads(ReceivedResponse())
  private implicit val receivedResponseWrites = emptyObjectWrites[ReceivedResponse]

  private implicit val killRequestReads = emptyObjectReads(KillServerRequest())
  private implicit val killRequestWrites = emptyObjectWrites[KillServerRequest]

  private implicit val taskLogEventReads = Json.reads[TaskLogEvent]
  private implicit val taskLogEventWrites = Json.writes[TaskLogEvent]
  private implicit val coreLogEventReads = Json.reads[CoreLogEvent]
  private implicit val coreLogEventWrites = Json.writes[CoreLogEvent]
  private implicit val backgroundJobLogEventReads = Json.reads[BackgroundJobLogEvent]
  private implicit val backgroundJobLogEventWrites = Json.writes[BackgroundJobLogEvent]
  private implicit val cancelExecutionRequestReads = Json.reads[CancelExecutionRequest]
  private implicit val cancelExecutionRequestWrites = Json.writes[CancelExecutionRequest]
  private implicit val cancelExecutionResponseReads = Json.reads[CancelExecutionResponse]
  private implicit val cancelExecutionResponseWrites = Json.writes[CancelExecutionResponse]
  private implicit val clientInfoReads = Json.reads[ClientInfo]
  private implicit val clientInfoWrites = Json.writes[ClientInfo]
  private implicit val registerClientRequestReads = Json.reads[RegisterClientRequest]
  private implicit val registerClientRequestWrites = Json.writes[RegisterClientRequest]
  private implicit val executionRequestReads = Json.reads[ExecutionRequest]
  private implicit val executionRequestWrites = Json.writes[ExecutionRequest]
  private implicit val keyExecutionRequestReads = Json.reads[KeyExecutionRequest]
  private implicit val keyExecutionRequestWrites = Json.writes[KeyExecutionRequest]
  private implicit val executionReceivedReads = Json.reads[ExecutionRequestReceived]
  private implicit val executionReceivedWrites = Json.writes[ExecutionRequestReceived]
  private implicit val executionWaitingReads = Json.reads[ExecutionWaiting]
  private implicit val executionWaitingWrites = Json.writes[ExecutionWaiting]
  private implicit val executionStartingReads = Json.reads[ExecutionStarting]
  private implicit val executionStartingWrites = Json.writes[ExecutionStarting]
  private implicit val executionSuccessReads = Json.reads[ExecutionSuccess]
  private implicit val executionSuccessWrites = Json.writes[ExecutionSuccess]
  private implicit val executionFailureReads = Json.reads[ExecutionFailure]
  private implicit val executionFailureWrites = Json.writes[ExecutionFailure]
  private implicit val listenToEventsReads = emptyObjectReads(ListenToEvents())
  private implicit val listenToEventsWrites = emptyObjectWrites[ListenToEvents]
  private implicit val unlistenToEventsReads = emptyObjectReads(UnlistenToEvents())
  private implicit val unlistenToEventsWrites = emptyObjectWrites[UnlistenToEvents]
  private implicit val listenToBuildChangeReads = emptyObjectReads(ListenToBuildChange())
  private implicit val listenToBuildChangeWrites = emptyObjectWrites[ListenToBuildChange]
  private implicit val unlistenToBuildChangeReads = emptyObjectReads(UnlistenToBuildChange())
  private implicit val unlistenToBuildChangeWrites = emptyObjectWrites[UnlistenToBuildChange]
  private implicit val sendSyntheticBuildChangedReads = emptyObjectReads(SendSyntheticBuildChanged())
  private implicit val sendSyntheticBuildChangedWrites = emptyObjectWrites[SendSyntheticBuildChanged]
  private implicit val buildStructureChangedReads = Json.reads[BuildStructureChanged]
  private implicit val buildStructureChangedWrites = Json.writes[BuildStructureChanged]
  private implicit val listenToValueReads = Json.reads[ListenToValue]
  private implicit val listenToValueWrites = Json.writes[ListenToValue]
  private implicit val unlistenToValueReads = Json.reads[UnlistenToValue]
  private implicit val unlistenToValueWrites = Json.writes[UnlistenToValue]
  private implicit val sendSyntheticValueChangedReads = Json.reads[SendSyntheticValueChanged]
  private implicit val sendSyntheticValueChangedWrites = Json.writes[SendSyntheticValueChanged]
  private implicit val keyNotFoundReads = Json.reads[KeyNotFound]
  private implicit val keyNotFoundWrites = Json.writes[KeyNotFound]
  private implicit val taskStartedReads = Json.reads[TaskStarted]
  private implicit val taskStartedWrites = Json.writes[TaskStarted]
  private implicit val taskFinishedReads = Json.reads[TaskFinished]
  private implicit val taskFinishedWrites = Json.writes[TaskFinished]
  private implicit val readLineRequestReads = Json.reads[ReadLineRequest]
  private implicit val readLineRequestWrites = Json.writes[ReadLineRequest]
  private implicit val readLineResponseReads = Json.reads[ReadLineResponse]
  private implicit val readLineResponseWrites = Json.writes[ReadLineResponse]
  private implicit val confirmRequestReads = Json.reads[ConfirmRequest]
  private implicit val confirmRequestWrites = Json.writes[ConfirmRequest]
  private implicit val confirmResponseReads = Json.reads[ConfirmResponse]
  private implicit val confirmResponseWrites = Json.writes[ConfirmResponse]
  private implicit val keyLookupRequestReads = Json.reads[KeyLookupRequest]
  private implicit val keyLookupRequestWrites = Json.writes[KeyLookupRequest]
  private implicit val keyLookupResponseReads = Json.reads[KeyLookupResponse]
  private implicit val keyLookupResponseWrites = Json.writes[KeyLookupResponse]
  private implicit val analyzeExecutionRequestReads = Json.reads[AnalyzeExecutionRequest]
  private implicit val analyzeExecutionRequestWrites = Json.writes[AnalyzeExecutionRequest]
  private implicit val analyzeExecutionResponseReads = Json.reads[AnalyzeExecutionResponse]
  private implicit val analyzeExecutionResponseWrites = Json.writes[AnalyzeExecutionResponse]
  private implicit val buildLoadedReads = emptyObjectReads(BuildLoaded())
  private implicit val buildLoadedWrites = emptyObjectWrites[BuildLoaded]
  private implicit val buildFailedToLoadReads = emptyObjectReads(BuildFailedToLoad())
  private implicit val buildFailedToLoadWrites = emptyObjectWrites[BuildFailedToLoad]
  private implicit val backgroundJobInfoReads = Json.reads[BackgroundJobInfo]
  private implicit val backgroundJobInfoWrites = Json.writes[BackgroundJobInfo]
  private implicit val backgroundJobStartedReads = Json.reads[BackgroundJobStarted]
  private implicit val backgroundJobStartedWrites = Json.writes[BackgroundJobStarted]
  private implicit val backgroundJobFinishedReads = Json.reads[BackgroundJobFinished]
  private implicit val backgroundJobFinishedWrites = Json.writes[BackgroundJobFinished]

  // This needs a custom formatter because it has a custom apply/unapply
  // which confuses the auto-formatter macro
  private implicit val taskEventWrites: Writes[TaskEvent] = (
    (__ \ "taskId").write[Long] and
    (__ \ "name").write[String] and
    (__ \ "serialized").write[JsValue])(unlift(TaskEvent.unapply))

  private implicit val taskEventReads: Reads[TaskEvent] = (
    (__ \ "taskId").read[Long] and
    (__ \ "name").read[String] and
    (__ \ "serialized").read[JsValue])((id, name, serialized) => TaskEvent(id, name, serialized))

  private implicit val valueChangedReads = Json.reads[ValueChanged]

  private implicit val valueChangedWrites = Json.writes[ValueChanged]

  // This needs a custom formatter because it has a custom apply/unapply
  // which confuses the auto-formatter macro
  private implicit val backgroundJobEventWrites = Writes[BackgroundJobEvent] { event =>
    Json.obj("jobId" -> event.jobId, "name" -> event.name, "serialized" -> event.serialized)
  }

  private implicit val backgroundJobEventReads = Reads[BackgroundJobEvent] { v =>
    for {
      jobId <- (v \ "jobId").validate[Long]
      name <- (v \ "name").validate[String]
      serialized = (v \ "serialized")
    } yield BackgroundJobEvent(jobId = jobId, name = name, serialized = serialized)
  }

  private implicit val completionReads = Json.reads[Completion]
  private implicit val completionWrites = Json.writes[Completion]
  private implicit val commandCompletionsRequestReads = Json.reads[CommandCompletionsRequest]
  private implicit val commandCompletionsRequestWrites = Json.writes[CommandCompletionsRequest]
  private implicit val commandCompletionsResponseReads = Json.reads[CommandCompletionsResponse]
  private implicit val commandCompletionsResponseWrites = Json.writes[CommandCompletionsResponse]

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

  private def msg[T <: Message](implicit f: Format[T], mf: ClassManifest[T]): (Class[T], (String, Reads[T], Writes[T])) =
    mf.runtimeClass.asInstanceOf[Class[T]] -> (makeSimpleName(mf.runtimeClass), f, f)

  private val messages: Map[Class[_], (String, Reads[_], Writes[_])] = Map(
    msg[TaskLogEvent],
    msg[CoreLogEvent],
    msg[BackgroundJobLogEvent],
    msg[KillServerRequest],
    msg[CancelExecutionRequest],
    msg[CancelExecutionResponse],
    msg[RegisterClientRequest],
    msg[ReadLineRequest],
    msg[ReadLineResponse],
    msg[ConfirmRequest],
    msg[ConfirmResponse],
    msg[ReceivedResponse],
    msg[ExecutionRequestReceived],
    msg[ExecutionRequest],
    msg[KeyExecutionRequest],
    msg[ExecutionStarting],
    msg[ExecutionWaiting],
    msg[ExecutionFailure],
    msg[ExecutionSuccess],
    msg[ListenToEvents],
    msg[UnlistenToEvents],
    msg[ListenToBuildChange],
    msg[UnlistenToBuildChange],
    msg[SendSyntheticBuildChanged],
    msg[ListenToValue],
    msg[UnlistenToValue],
    msg[SendSyntheticValueChanged],
    msg[KeyNotFound],
    msg[BuildStructureChanged],
    msg[ValueChanged],
    msg[ErrorResponse],
    msg[TaskStarted],
    msg[TaskFinished],
    msg[CommandCompletionsRequest],
    msg[CommandCompletionsResponse],
    msg[KeyLookupResponse],
    msg[KeyLookupRequest],
    msg[AnalyzeExecutionRequest],
    msg[AnalyzeExecutionResponse],
    msg[TaskEvent],
    msg[BuildLoaded],
    msg[BuildFailedToLoad],
    msg[BackgroundJobStarted],
    msg[BackgroundJobFinished],
    msg[BackgroundJobEvent])

  private val readsIndex: Map[String, Reads[_]] =
    (for {
      (_, (name, reads, _)) <- messages
    } yield name -> reads).toMap

  private def addType(json: JsValue, name: String): JsObject = json match {
    case x: JsObject => x + ("type" -> JsString(name))
    case value => sys.error("Unable to serialize non-object message type!")
  }

  object messageWrites extends Writes[Message] {
    override def writes(t: Message): JsValue = {
      val (name, _, writes) = try messages(t.getClass) catch {
        case e: NoSuchElementException =>
          throw new RuntimeException(s"No message writer known for ${t.getClass.getName}")
      }
      // TODO - Should the message field be something like "event" or "request"?
      addType(writes.asInstanceOf[Writes[Message]].writes(t), name)
    }
  }

  object messageReads extends Reads[Message] {
    // if we mess up a read/write pair we just get a cache miss, no big deal
    @volatile
    private var cache = Map.empty[String, Reads[_]]
    override def reads(msg: JsValue): JsResult[Message] = {
      val name = (msg \ "type").as[String]
      val reader = cache.get(name) orElse {
        readsIndex.get(name).map { created =>
          cache += (name -> created)
          created
        }
      } getOrElse { throw new RuntimeException(s"No message reader known for $name") }
      reader.reads(msg).map(_.asInstanceOf[Message])
    }
  }
}
