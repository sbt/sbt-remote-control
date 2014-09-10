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

  private val messages: Map[Class[_], (String, DynamicSerialization => Reads[_], Writes[_])] = Map(
    msg[TaskLogEvent],
    msg[CoreLogEvent],
    msg[BackgroundJobLogEvent],
    msg[KillServerRequest],
    msg[CancelExecutionRequest],
    msg[CancelExecutionResponse],
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
    msg[BuildStructureChanged],
    msg[ValueChanged[Any]] { serializations: DynamicSerialization =>
      implicit val buildValueReads = BuildValue.reads[Any](serializations)
      implicit val taskResultReads = TaskResult.reads[Any] // this line shouldn't be needed...
      valueChangedReads
    },
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
  private val readsIndex: Map[String, DynamicSerialization => Reads[_]] =
    (for {
      (_, (name, readFactory, _)) <- messages
    } yield name -> readFactory).toMap

  private object messageWrites extends Writes[Message] {
    override def writes(t: Message): JsValue = {
      val (name, _, writes) = try messages(t.getClass) catch {
        case e: NoSuchElementException =>
          throw new RuntimeException(s"No message writer known for ${t.getClass.getName}")
      }
      // TODO - Should the message field be something like "event" or "request"?
      addType(writes.asInstanceOf[Writes[Message]].writes(t), name)
    }
  }

  private class MessageReads(val serializations: DynamicSerialization) extends Reads[Message] {
    // if we mess up a read/write pair we just get a cache miss, no big deal
    @volatile
    private var cache = Map.empty[String, Reads[_]]
    override def reads(msg: JsValue): JsResult[Message] = {
      val name = (msg \ "type").as[String]
      val reader = cache.get(name) orElse {
        readsIndex.get(name).map(factory => factory(serializations)).map { created =>
          cache += (name -> created)
          created
        }
      } getOrElse { throw new RuntimeException(s"No message reader known for $name") }
      reader.reads(msg).map(_.asInstanceOf[Message])
    }
  }

  private class MessageFormat(serializations: DynamicSerialization) extends Format[Message] {
    def writes(t: Message): JsValue =
      messageWrites.writes(t)

    val reader = new MessageReads(serializations)

    def reads(msg: JsValue): JsResult[Message] =
      reader.reads(msg)
  }

  // we usually use the same serializations for a bunch of values at once
  // so just keep around this object
  @volatile
  private var oneItemCache: Option[MessageFormat] = None

  private def getFormat(serializations: DynamicSerialization): MessageFormat = {
    oneItemCache.filter(fmt => fmt.reader.serializations eq serializations).getOrElse {
      val created = new MessageFormat(serializations)
      // tasty side effects
      oneItemCache = Some(created)
      created
    }
  }

  private def addType(json: JsValue, name: String): JsObject = json match {
    case x: JsObject => x + ("type" -> JsString(name))
    case value => sys.error("Unable to serialize non-object message type!")
  }

  private def msg[T <: Message](implicit f: Format[T], mf: ClassManifest[T]): (Class[T], (String, DynamicSerialization => Reads[T], Writes[T])) =
    mf.runtimeClass.asInstanceOf[Class[T]] -> (simpleName(mf.runtimeClass), _ => f, f)

  private def msg[T <: Message](readsFactory: DynamicSerialization => Reads[T])(implicit writes: Writes[T], mf: ClassManifest[T]): (Class[T], (String, DynamicSerialization => Reads[T], Writes[T])) =
    mf.runtimeClass.asInstanceOf[Class[T]] -> (simpleName(mf.runtimeClass), readsFactory, writes)

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

  // just used below to go WireEnvelope => Envelope, and in test suite
  def fromRaw(msg: JsValue, serializations: DynamicSerialization): Option[Message] =
    getFormat(serializations).reads(msg).asOpt

  // just used by the test suite
  def toRaw(msg: Message): JsValue =
    messageWrites.writes(msg)

  val sendJsonFilter: (Any, JsValue) => JsValue = { (msg: Any, json: JsValue) =>
    msg match {
      case m: Message =>
        // try to avoid recomputing the name if it's a known class
        val name = try messages(msg.getClass)._1 catch {
          case e: NoSuchElementException => simpleName(msg.getClass)
        }
        addType(json, name)
      case other =>
        throw new RuntimeException("you can only send a Message, not " + msg)
    }
  }

}

final case class Envelope(override val serial: Long, override val replyTo: Long, override val content: Message) extends ipc.Envelope[Message]

/**
 * This class is responsible for extracting from the wire protocol into
 *  the "class" protocol.  This may disappear at some point, as the duplication with ipc.Envelope may not be necessary.
 */
object Envelope {
  def apply(wire: ipc.WireEnvelope, serializations: DynamicSerialization): Envelope = {
    val message: Message = try {
      // this can throw malformed json errors
      WireProtocol.fromRaw(Json.parse(wire.asString), serializations).getOrElse(sys.error("Failure deserializing json."))
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
