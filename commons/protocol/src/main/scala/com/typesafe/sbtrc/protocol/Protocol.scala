package com.typesafe.sbtrc.protocol

import com.typesafe.sbtrc.ipc
import scala.util.parsing.json._
import com.typesafe.sbtrc.ipc.JsonReader
import java.io.File

sealed trait LogEntry {
  def message: String
}

case class LogStdOut(message: String) extends LogEntry
case class LogStdErr(message: String) extends LogEntry
case class LogSuccess(message: String) extends LogEntry
case class LogTrace(throwableClass: String, message: String) extends LogEntry
case class LogMessage(level: String, message: String) extends LogEntry {
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

object LogEntry {
  implicit object JsonRepresentationOfLogEntry extends ipc.JsonRepresentation[LogEntry] {
    override def toJson(entry: LogEntry): JSONObject = {
      val obj = entry match {
        case LogSuccess(message) =>
          Map("type" -> "success", "message" -> message)
        case LogTrace(klass, message) =>
          Map("type" -> "trace", "class" -> klass, "message" -> message)
        case LogMessage(level, message) =>
          Map("type" -> "message", "level" -> level, "message" -> message)
        case LogStdOut(message) =>
          Map("type" -> "stdout", "message" -> message)
        case LogStdErr(message) =>
          Map("type" -> "stderr", "message" -> message)

      }
      JSONObject(obj)
    }

    override def fromJson(j: JSONType): LogEntry = {
      j match {
        case JSONObject(obj) =>
          obj("type") match {
            case "success" => LogSuccess(obj("message").asInstanceOf[String])
            case "trace" => LogTrace(obj("class").asInstanceOf[String], obj("message").asInstanceOf[String])
            case "message" => LogMessage(obj("level").asInstanceOf[String], obj("message").asInstanceOf[String])
            case "stdout" => LogStdOut(obj("message").asInstanceOf[String])
            case "stderr" => LogStdErr(obj("message").asInstanceOf[String])
            case whatever =>
              throw new Exception("unexpected LogEntry type: " + whatever)
          }
        case JSONArray(list) =>
          throw new Exception("not expecting JSONArray: " + list)
      }
    }
  }
}

// This type needs to be completely/permanently split into
// SpecificMessage and GenericMessage for better safety.
// Will come in a future refactoring round.
sealed trait Message {
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
  def jsonTypeString = removeDollar(lastChunk(getClass.getName))
}

sealed trait SpecificMessage extends Message {
  def toGeneric: GenericMessage
}

// FOR NOW we always use this on the wire and always
// use SpecificMessage elsewhere for type safety;
// but later on we want to remove special-case handling
// for SpecificMessage and then we would avoid converting
// to generic.
sealed trait GenericMessage extends Message {
  def toSpecific: Option[SpecificMessage]
}

sealed trait Request extends Message {
  // whether to send events between request/response, in
  // addition to just the response.
  def sendEvents: Boolean
}
// we use GenericRequest on the wire and parse to SpecificRequest
// when we want type safety
sealed trait SpecificRequest extends Request with SpecificMessage {
  override def toGeneric: GenericRequest
}
sealed trait Response extends Message
// we use GenericResponse on the wire and parse to SpecificResponse
// when we want type safety
sealed trait SpecificResponse extends Response with SpecificMessage {
  override def toGeneric: GenericResponse
}
sealed trait Event extends Message
// we use GenericEvent on the wire and parse to SpecificEvent
// when we want type safety
sealed trait SpecificEvent extends Event with SpecificMessage {
  override def toGeneric: GenericEvent
}

case object CancelRequest extends Request {
  override def sendEvents = false
}

case object CancelResponse extends Response

object TaskNames {
  val name = "name"
  val discoveredMainClasses = "discovered-main-classes"
  val watchTransitiveSources = "watch-transitive-sources"
  val compile = "compile"
  val run = "run"
  val runMain = "run-main"
  val test = "test"
}

case class GenericRequest(sendEvents: Boolean, name: String, params: Map[String, Any]) extends Request with GenericMessage {
  override def toSpecific: Option[SpecificRequest] = {
    Option(name match {
      case TaskNames.name => NameRequest(sendEvents = sendEvents)
      case TaskNames.discoveredMainClasses => DiscoveredMainClassesRequest(sendEvents = sendEvents)
      case TaskNames.watchTransitiveSources => WatchTransitiveSourcesRequest(sendEvents = sendEvents)
      case TaskNames.compile => CompileRequest(sendEvents = sendEvents)
      case TaskNames.run => RunRequest(sendEvents = sendEvents, mainClass = None)
      case TaskNames.runMain => RunRequest(sendEvents = sendEvents, mainClass = params.get("mainClass").map(_.asInstanceOf[String]))
      case TaskNames.test => TestRequest(sendEvents = sendEvents)
      case _ =>
        null
    })
  }
}

case class GenericResponse(name: String, params: Map[String, Any]) extends Response with GenericMessage {
  override def toSpecific: Option[SpecificResponse] = {
    Option(name match {
      case TaskNames.name => NameResponse(params("name").asInstanceOf[String])
      case TaskNames.discoveredMainClasses => DiscoveredMainClassesResponse(params("names").asInstanceOf[Seq[String]])
      case TaskNames.watchTransitiveSources => WatchTransitiveSourcesResponse(params("files").asInstanceOf[Seq[String]].map(new File(_)))
      case TaskNames.compile => CompileResponse(params("success").asInstanceOf[Boolean])
      case TaskNames.run => RunResponse(params("success").asInstanceOf[Boolean], TaskNames.run)
      case TaskNames.runMain => RunResponse(params("success").asInstanceOf[Boolean], TaskNames.runMain)
      case TaskNames.test => TestResponse(outcome = TestOutcome(params("outcome").asInstanceOf[String]))
      case _ =>
        null
    })
  }
}

case class NameRequest(sendEvents: Boolean) extends SpecificRequest {
  override def toGeneric = GenericRequest(sendEvents = sendEvents,
    name = TaskNames.name,
    Map.empty)
}
case class NameResponse(name: String) extends SpecificResponse {
  override def toGeneric = GenericResponse(TaskNames.name, Map("name" -> name))
}

case class DiscoveredMainClassesRequest(sendEvents: Boolean) extends SpecificRequest {
  override def toGeneric = GenericRequest(sendEvents = sendEvents, TaskNames.discoveredMainClasses, Map.empty)
}
case class DiscoveredMainClassesResponse(names: Seq[String]) extends SpecificResponse {
  override def toGeneric = GenericResponse(TaskNames.discoveredMainClasses, Map("names" -> names.toList))
}

case class WatchTransitiveSourcesRequest(sendEvents: Boolean) extends SpecificRequest {
  override def toGeneric = GenericRequest(sendEvents = sendEvents, TaskNames.watchTransitiveSources, Map.empty)
}
case class WatchTransitiveSourcesResponse(files: Seq[File]) extends SpecificResponse {
  override def toGeneric = GenericResponse(TaskNames.watchTransitiveSources, Map("files" -> files.map(_.getPath).toList))
}

case class CompileRequest(sendEvents: Boolean) extends SpecificRequest {
  override def toGeneric = GenericRequest(sendEvents = sendEvents, TaskNames.compile, Map.empty)
}
case class CompileResponse(success: Boolean) extends SpecificResponse {
  override def toGeneric = GenericResponse(TaskNames.compile, Map("success" -> success))
}

case class RunRequest(sendEvents: Boolean, mainClass: Option[String]) extends SpecificRequest {
  override def toGeneric = GenericRequest(sendEvents = sendEvents, mainClass.map(_ => TaskNames.runMain).getOrElse(TaskNames.run),
    mainClass.map(mc => Map("mainClass" -> mc)).getOrElse(Map.empty))
}
case class RunResponse(success: Boolean, task: String) extends SpecificResponse {
  override def toGeneric = GenericResponse(task, Map("success" -> success))
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

object TestOutcome {
  def apply(s: String): TestOutcome = s match {
    case "passed" => TestPassed
    case "failed" => TestFailed
    case "error" => TestError
    case "skipped" => TestSkipped
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

case class TestRequest(sendEvents: Boolean) extends SpecificRequest {
  override def toGeneric = GenericRequest(sendEvents = sendEvents, TaskNames.test, Map.empty)
}
case class TestResponse(outcome: TestOutcome) extends SpecificResponse {
  def success: Boolean = outcome.success
  override def toGeneric = GenericResponse(TaskNames.test, Map("outcome" -> outcome.toString))
}

// can be the response to anything
case class ErrorResponse(error: String) extends Response

// exactly one of the boot events is sent on startup
sealed trait BootEvent extends Event
// we need to restart sbt in an orderly fashion
case object NeedRebootEvent extends BootEvent
// we successfully booted up
case object NowListeningEvent extends BootEvent

// when we receive a request but before we process it, we send this
case object RequestReceivedEvent extends Event
case class LogEvent(entry: LogEntry) extends Event

// pseudo-wire-messages we synthesize locally
case object Started extends Event
case object Stopped extends Event

// should not happen, basically
case class MysteryMessage(something: Any) extends Event

case class GenericEvent(task: String, id: String, params: Map[String, Any]) extends Event with GenericMessage {
  override def toSpecific: Option[SpecificEvent] = {
    Option(task match {
      case TaskNames.test => id match {
        case "result" => TestEvent(params("name").asInstanceOf[String],
          params.get("description").map(_.asInstanceOf[String]),
          TestOutcome(params("outcome").asInstanceOf[String]),
          params.get("error").map(_.asInstanceOf[String]))
        case _ => null
      }
      case _ => null
    })
  }
}

case class TestEvent(name: String, description: Option[String], outcome: TestOutcome, error: Option[String]) extends SpecificEvent {
  override def toGeneric = GenericEvent(task = TaskNames.test, id = "result",
    Map("name" -> name, "outcome" -> outcome.toString) ++
      description.map(desc => Map("description" -> desc)).getOrElse(Map.empty) ++
      error.map(e => Map("error" -> e)).getOrElse(Map.empty))
}

object Message {
  private def cleanJsonFromAny(value: Any): Any = value match {
    // null is allowed in json
    case null => null
    // strip the scala JSON wrappers off, if present; this
    // is basically due to not being sure when Scala's json stuff
    // will use these.
    case JSONObject(v) => cleanJsonFromAny(v)
    case JSONArray(v) => cleanJsonFromAny(v)
    // all sequences must be lists of sanitized values
    case s: Seq[_] => s.map(cleanJsonFromAny(_)).toList
    case m: Map[_, _] => m map {
      case (key: String, value) =>
        (key -> cleanJsonFromAny(value))
      case whatever =>
        throw new RuntimeException("Invalid map entry in params " + whatever)
    }
    case s: String => s
    case n: Number => n
    case b: Boolean => b
    case whatever => throw new RuntimeException("not allowed in params: " + whatever)
  }

  private def cleanJsonFromParams(params: Any): Map[String, Any] = {
    cleanJsonFromAny(params).asInstanceOf[Map[String, Any]]
  }

  private def addJsonToAny(value: Any): Any = value match {
    // null is allowed in json
    case null => null
    // keep wrappers but ensure we wrap any nested items
    case JSONObject(v) => addJsonToAny(v)
    case JSONArray(v) => addJsonToAny(v)
    // add wrappers if missing
    case s: Seq[_] => JSONArray(s.map(addJsonToAny(_)).toList)
    case m: Map[_, _] => JSONObject(m map {
      case (key: String, value) =>
        (key -> addJsonToAny(value))
      case whatever =>
        throw new RuntimeException("Invalid map entry in params " + whatever)
    })
    case s: String => s
    case n: Number => n
    case b: Boolean => b
    case whatever => throw new RuntimeException("not allowed in params: " + whatever)
  }

  private def addJsonToParams(params: Any): JSONObject = {
    addJsonToAny(params).asInstanceOf[JSONObject]
  }

  private def fromParams(params: Map[String, Any]): JSONObject = {
    addJsonToParams(params)
  }

  def paramsToJsonString(params: Map[String, Any]): String = {
    fromParams(params).toString
  }

  def paramsFromJsonString(json: String): Map[String, Any] = {
    JSON.parseFull(json) match {
      case Some(obj: Map[_, _]) => cleanJsonFromParams(obj)
      case whatever =>
        throw new Exception("JSON parse failure on: " + json + " parsed: " + whatever)
    }
  }

  implicit object JsonRepresentationOfMessage extends ipc.JsonRepresentation[Message] {

    override def toJson(m: Message): JSONObject = {
      import scala.util.parsing.json._

      m match {
        // always serialize as generic
        case specific: SpecificMessage => toJson(specific.toGeneric)
        case other => {
          val base = Map("type" -> m.jsonTypeString)
          val obj: Map[String, Any] = m match {
            case req: Request =>
              base ++ Map("sendEvents" -> req.sendEvents) ++ {
                req match {
                  case GenericRequest(_, name, params) =>
                    Map("name" -> name, "params" -> fromParams(params))
                  case _ =>
                    Map.empty[String, Any]
                }
              }
            case Started | Stopped | RequestReceivedEvent | NeedRebootEvent | NowListeningEvent =>
              base
            case GenericResponse(name, params) =>
              base ++ Map("name" -> name, "params" -> fromParams(params))
            case GenericEvent(task, id, params) =>
              base ++ Map("task" -> task, "id" -> id, "params" -> fromParams(params))
            case ErrorResponse(error) =>
              base ++ Map("error" -> error)
            case MysteryMessage(something) =>
              base ++ Map("something" -> something.toString)
            case LogEvent(entry) =>
              base ++ Map("entry" -> implicitly[ipc.JsonWriter[LogEntry]].toJson(entry))
            case whatever =>
              throw new Exception("Need to implement JSON serialization of: " + whatever)
          }
          JSONObject(obj)
        }
      }
    }

    private def parseLogList(obj: Map[String, Any], key: String): List[LogEntry] = {
      val reader = implicitly[JsonReader[LogEntry]]
      obj(key) match {
        case list: List[_] =>
          list collect {
            case m: Map[_, _] => reader.fromJson(JSONObject(m.asInstanceOf[Map[String, _]]))
          }
        case whatever =>
          throw new Exception("expecting a json array of log entry, got: " + whatever)
      }
    }

    override def fromJson(json: JSONType): Message = {
      // "sendEvents" is optional so in a JSON API it can be defaulted to true
      def getSendEvents(obj: Map[String, Any]): Boolean =
        obj.get("sendEvents").map(_.asInstanceOf[Boolean]).getOrElse(true)
      json match {
        case JSONObject(obj) =>
          obj("type") match {
            case "CancelRequest" =>
              CancelRequest
            case "CancelResponse" =>
              CancelResponse
            case "GenericRequest" =>
              GenericRequest(sendEvents = getSendEvents(obj), name = obj("name").asInstanceOf[String],
                params = cleanJsonFromParams(obj.get("params").getOrElse(Map.empty)))
            case "GenericResponse" =>
              GenericResponse(name = obj("name").asInstanceOf[String], params = cleanJsonFromParams(obj.get("params").getOrElse(Map.empty)))
            case "GenericEvent" =>
              GenericEvent(task = obj("task").asInstanceOf[String], id = obj("id").asInstanceOf[String],
                params = cleanJsonFromParams(obj.get("params").getOrElse(Map.empty)))
            case "ErrorResponse" =>
              ErrorResponse(obj("error").asInstanceOf[String])
            case "Started" =>
              Started
            case "Stopped" =>
              Stopped
            case "NeedRebootEvent" =>
              NeedRebootEvent
            case "NowListeningEvent" =>
              NowListeningEvent
            case "RequestReceivedEvent" =>
              RequestReceivedEvent
            case "MysteryMessage" =>
              MysteryMessage(obj("something").asInstanceOf[String])
            case "LogEvent" =>
              LogEvent(implicitly[JsonReader[LogEntry]].fromJson(JSONObject(obj("entry").asInstanceOf[Map[String, Any]])))
            case whatever =>
              throw new Exception("unknown message type in json: " + whatever)
          }
        case JSONArray(list) =>
          throw new Exception("not expecting a json list")
      }
    }
  }
}

case class Envelope(override val serial: Long, override val replyTo: Long, override val content: Message) extends ipc.Envelope[Message]

object Envelope {
  def apply(wire: ipc.WireEnvelope): Envelope = {
    val message = try {
      val json = JSON.parseFull(wire.asString) match {
        case Some(obj: Map[_, _]) => JSONObject(obj.asInstanceOf[Map[String, _]])
        case whatever =>
          throw new Exception("JSON parse failure on: " + wire.asString + " parsed: " + whatever)
      }
      // this can throw malformed json errors
      implicitly[JsonReader[Message]].fromJson(json)
    } catch {
      case e: Exception =>
        //System.err.println("**** " + e.getMessage)
        //System.err.println(e.getStackTraceString)
        // probably a JSON parse failure
        if (wire.replyTo != 0L)
          ErrorResponse("exception parsing json: " + e.getClass.getSimpleName + ": " + e.getMessage)
        else
          MysteryMessage(try wire.asString catch { case e: Exception => wire })
    }

    Envelope(wire.serial, wire.replyTo, message)
  }
}
