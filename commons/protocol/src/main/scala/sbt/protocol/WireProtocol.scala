package sbt.protocol

import com.typesafe.sbtrc.ipc
import play.api.libs.json._
import java.io.File
import language.existentials

/**
 * This object helps us serialize/deserialize messages into raw formats.
 * 
 * It contains all the logic about precedence and hard-coded knowledge of specific message types we can
 * serialize/deserialize.
 * 
 * TODO - Should you be able to register more messages to serialize here?
 * TODO - Should you register the BuildValue serializer/deserializers here?
 */
object WireProtocol {

  private val messages: Map[Class[_], (String, Format[_])] = Map(
    msg[RegisterClientRequest],
    msg[RequestCompleted],
    msg[RequestFailed],
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
    msg[LogEvent],
    msg[TestEvent],
    msg[BuildStructureChanged],
    msg[ValueChanged[Any]],
    msg[ErrorResponse],
    msg[CompilationFailure],
    msg[TaskStarted],
    msg[TaskFinished],
    msg[CommandCompletionsRequest],
    msg[CommandCompletionsResponse],
    msg[KeyLookupResponse],
    msg[KeyLookupRequest]
  )
  private val lookUpIndex: Map[String, Format[_]] =
    (for {
      (_, (name, format)) <- messages
    } yield name -> format).toMap
   // Here' we implement protocol deserialization using the RawStructure
  // typeclass....
  // TODO - Implement...
  implicit object messageFormat extends Format[Message] {   
    def writes(t: Message): JsValue = {
      val (name, out) = try messages(t.getClass) catch {
        case e: NoSuchElementException =>
          throw new RuntimeException(s"No message writer known for ${t.getClass.getName}")
      }
      // TODO - Should the message field be something like "event" or "request"?
      out.asInstanceOf[Format[Message]].writes(t) match {
        case x: JsObject => x + ("type" -> JsString(name))
        case value => sys.error("Unable to serialize non-object message type!")
      }
    }
    def reads(msg: JsValue): JsResult[Message] = {
      val name = (msg \ "type").as[String]
      try lookUpIndex(name).reads(msg).asInstanceOf[JsResult[Message]]
      catch {
        case e: NoSuchElementException =>
          throw new RuntimeException(s"No message reader known for $name", e)
      }
    }

  }


  
  private def msg[T <: Message](implicit f: Format[T], mf: ClassManifest[T]): (Class[T], (String, Format[T])) =
    mf.runtimeClass.asInstanceOf[Class[T]] -> (simpleName(mf.runtimeClass) -> f)


  private def removeDollar(s: String) = {
    val i = s.lastIndexOf('$')
    if (i >= 0)
      s.substring(0, i)
    else
      s
  }
  private def lastChunk(s: String) = {
    val i = s.lastIndexOf('.')
    if (i >= 0)
      s.substring(i + 1)
    else
      s
  }
  private def simpleName(c: Class[_]) = removeDollar(lastChunk(c.getName))


  def fromRaw(msg: JsValue): Option[Message] = 
    messageFormat.reads(msg).asOpt
  def toRaw(msg: Message): JsValue = 
    messageFormat.writes(msg)
}


case class Envelope(override val serial: Long, override val replyTo: Long, override val content: Message) extends ipc.Envelope[Message]

/** This class is responsible for extracting from the wire protocol into
 *  the "class" protocol.  This may disappear at some point, as the duplication with ipc.Envelope may not be necessary.
 */
object Envelope {
  def apply(wire: ipc.WireEnvelope): Envelope = {
    val message: Message = try {
      // this can throw malformed json errors
      WireProtocol.fromRaw(Json.parse(wire.asString)).getOrElse(sys.error("Failure deserializing json."))
    } catch {
      case e: Exception =>
        //System.err.println("**** " + e.getMessage)
        //System.err.println(e.getStackTraceString)
        // probably a JSON parse failure
        ErrorResponse("exception parsing json: " + e.getClass.getSimpleName + ": " + e.getMessage + "\n\nMsg: " + wire.asString)
        // TODO - Mysetery message?
    }
    new Envelope(wire.serial, wire.replyTo, message)
  }
  
}
