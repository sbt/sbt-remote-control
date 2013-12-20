package com.typesafe.sbtrc.protocol

import com.typesafe.sbtrc.ipc
import scala.util.parsing.json._
import com.typesafe.sbtrc.ipc.JsonReader
import java.io.File

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
   // Here' we implement protocol deserialization using the RawStructure
  // typeclass....
  implicit object MessageStructure extends RawStructure[Message] {
    val ExecutionRequestMsg = RawStructure.get[ExecutionRequest]
    val ListenToEventsMsg = RawStructure.get[ListenToEvents]
    
    val CancelRequestMsg = RawStructure.get[CancelRequest.type]
    val CancelResponseMsg = RawStructure.get[CancelResponse.type]
    val SettingKeyRequestMsg = RawStructure.get[SettingKeyRequest]
    val TaskKeyRequestMsg = RawStructure.get[TaskKeyRequest]
    val InputTaskKeyRequestMsg = RawStructure.get[InputTaskKeyRequest]
    val ErrorResponseMsg = RawStructure.get[ErrorResponse]
    val SettingValueRequestMsg = RawStructure.get[SettingValueRequest]
    val SettingValueResponseMsg = RawStructure.get[SettingValueResponse[Any]]
    val TaskValueRequestMsg = RawStructure.get[TaskValueRequest]
    val TaskValueResponseMsg = RawStructure.get[TaskValueResponse[Any]]
    val KeyListResponseMsg = RawStructure.get[KeyListResponse]
    val ExecuteCommandRequestMsg = RawStructure.get[ExecuteCommandRequest]
    val ExecuteCommandResponseMsg = RawStructure.get[ExecuteCommandResponse]
    
    val NameRequestMsg = RawStructure.get[NameRequest]
    val NameResponseMsg = RawStructure.get[NameResponse]
    val MainClassRequestMsg = RawStructure.get[MainClassRequest]
    val MainClassResponseMsg = RawStructure.get[MainClassResponse]
    val WatchTransitiveSourcesRequestMsg = RawStructure.get[WatchTransitiveSourcesRequest]
    val WatchTransitiveSourcesResponseMsg = RawStructure.get[WatchTransitiveSourcesResponse]
    val CompileRequestMsg = RawStructure.get[CompileRequest]
    val CompileResponseMsg = RawStructure.get[CompileResponse]
    val RunRequestMsg = RawStructure.get[RunRequest]
    val RunResponseMsg = RawStructure.get[RunResponse]
    val TestRequestMsg = RawStructure.get[TestRequest]
    val TestResponseMsg = RawStructure.get[TestResponse]
    
    
    
    val LogEventMsg = RawStructure.get[LogEvent]
    val StartedEventMsg = RawStructure.get[Started.type]
    val StoppedEventMsg = RawStructure.get[Stopped.type]
    val NeedRebootEventMsg = RawStructure.get[NeedRebootEvent.type]
    val NowListeningEventMsg = RawStructure.get[NowListeningEvent.type]
    val RequestReceivedEventMsg = RawStructure.get[RequestReceivedEvent.type]
    val TestEventMsg = RawStructure.get[TestEvent]
    // Note: this must be parsed LAST.
    val GenericEventMsg = RawStructure.get[GenericEvent]
    
    def apply(t: Message): Map[String, Any] = t match {
      case x: ListenToEvents => ListenToEventsMsg(x)
      case x: ExecutionRequest => ExecutionRequestMsg(x)
      case CancelRequest => CancelRequestMsg(CancelRequest)
      case CancelResponse => CancelResponseMsg(CancelResponse)
      case x: SettingKeyRequest => SettingKeyRequestMsg(x)
      case x: TaskKeyRequest => TaskKeyRequestMsg(x)
      case x: InputTaskKeyRequest => InputTaskKeyRequestMsg(x)
      case x: ErrorResponse => ErrorResponseMsg(x)
      case x: SettingValueRequest => SettingValueRequestMsg(x)
      case x: SettingValueResponse[_] => SettingValueResponseMsg(x.asInstanceOf[SettingValueResponse[Any]])
      case x: TaskValueRequest => TaskValueRequestMsg(x)
      case x: TaskValueResponse[_] => TaskValueResponseMsg(x.asInstanceOf[TaskValueResponse[Any]])
      case x: KeyListResponse => KeyListResponseMsg(x)
      case x: ExecuteCommandRequest => ExecuteCommandRequestMsg(x)
      case x: ExecuteCommandResponse => ExecuteCommandResponseMsg(x)
      
      case x: NameRequest => NameRequestMsg(x)
      case x: NameResponse => NameResponseMsg(x)
      case x: MainClassRequest => MainClassRequestMsg(x)
      case x: MainClassResponse => MainClassResponseMsg(x)
      case x: WatchTransitiveSourcesRequest => WatchTransitiveSourcesRequestMsg(x)
      case x: WatchTransitiveSourcesResponse => WatchTransitiveSourcesResponseMsg(x)
      case x: CompileRequest => CompileRequestMsg(x)
      case x: CompileResponse => CompileResponseMsg(x)
      case x: RunRequest => RunRequestMsg(x)
      case x: RunResponse => RunResponseMsg(x)
      case x: TestRequest => TestRequestMsg(x)
      case x: TestResponse => TestResponseMsg(x)
      
      case x: LogEvent => LogEventMsg(x)
      case Started => StartedEventMsg(Started)
      case Stopped => StoppedEventMsg(Stopped)
      case NeedRebootEvent => NeedRebootEventMsg(NeedRebootEvent)
      case NowListeningEvent => NowListeningEventMsg(NowListeningEvent)
      case RequestReceivedEvent => RequestReceivedEventMsg(RequestReceivedEvent)
      case x: TestEvent => TestEventMsg(x)
      case x: GenericEvent => GenericEventMsg(x)
    }
    // TODO - Can we do this faster or less ugly?
    def unapply(msg: Map[String, Any]): Option[Message] = (
      ListenToEventsMsg.unapply(msg) orElse
      ExecutionRequestMsg.unapply(msg) orElse
      ErrorResponseMsg.unapply(msg) orElse
      CancelRequestMsg.unapply(msg) orElse
      SettingKeyRequestMsg.unapply(msg) orElse
      TaskKeyRequestMsg.unapply(msg) orElse
      InputTaskKeyRequestMsg.unapply(msg) orElse
      SettingValueRequestMsg.unapply(msg) orElse
      SettingValueResponseMsg.unapply(msg) orElse
      TaskValueRequestMsg.unapply(msg) orElse
      TaskValueResponseMsg.unapply(msg) orElse
      KeyListResponseMsg.unapply(msg) orElse
      ExecuteCommandRequestMsg.unapply(msg) orElse
      ExecuteCommandResponseMsg.unapply(msg) orElse
      
      NameRequestMsg.unapply(msg) orElse 
      NameResponseMsg.unapply(msg) orElse
      MainClassRequestMsg.unapply(msg) orElse
      MainClassResponseMsg.unapply(msg) orElse
      WatchTransitiveSourcesRequestMsg.unapply(msg) orElse
      WatchTransitiveSourcesResponseMsg.unapply(msg) orElse
      CompileRequestMsg.unapply(msg) orElse
      CompileResponseMsg.unapply(msg) orElse
      RunRequestMsg.unapply(msg) orElse
      RunResponseMsg.unapply(msg) orElse
      TestRequestMsg.unapply(msg) orElse
      TestResponseMsg.unapply(msg) orElse
      
      LogEventMsg.unapply(msg) orElse
      StartedEventMsg.unapply(msg) orElse
      StoppedEventMsg.unapply(msg) orElse
      NeedRebootEventMsg.unapply(msg) orElse
      NowListeningEventMsg.unapply(msg) orElse
      RequestReceivedEventMsg.unapply(msg) orElse
      TestEventMsg.unapply(msg) orElse
      // Note: This has to be the last parsed message to make sure we
      // pull specific events before dumping generic ones.
      GenericEventMsg.unapply(msg)
      
    )
  }  
  implicit val jsonWriter = ipc.JsonWriter.jsonWriter(MessageStructure)
  
  
  def fromRaw(msg: Map[String, Any]): Option[Message] = 
    MessageStructure.unapply(msg)
  def toRaw(msg: Message): Map[String, Any] = 
    MessageStructure(msg)
}


case class Envelope(override val serial: Long, override val replyTo: Long, override val content: Message) extends ipc.Envelope[Message]

/** This class is responsible for extracting from the wire protocol into
 *  the "class" protocol.  This may disappear at some point, as the duplication with ipc.Envelope may not be necessary.
 */
object Envelope {
  def apply(wire: ipc.WireEnvelope): Envelope = {
    val message: Message = try {
      val json = JSON.parseFull(wire.asString) match {
        case Some(obj: Map[_, _]) => JSONObject(obj.asInstanceOf[Map[String, _]])
        case whatever =>
          throw new Exception("JSON parse failure on: " + wire.asString + " parsed: " + whatever)
      }
      // this can throw malformed json errors
      val reader = JsonReader.fromRaw[Message](WireProtocol.MessageStructure)
      reader.fromJson(json)
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
    new Envelope(wire.serial, wire.replyTo, message)
  }
  
}