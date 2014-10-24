package sbt.protocol

import sbt.impl.ipc
import play.api.libs.json._
import java.io.File
import language.existentials

private[sbt] final case class Envelope(override val serial: Long, override val replyTo: Long, override val content: Message) extends ipc.Envelope[Message]

/**
 * This class is responsible for extracting from the wire protocol into
 *  the "class" protocol.  This may disappear at some point, as the duplication with ipc.Envelope may not be necessary.
 */
private[sbt] object Envelope {
  def apply(wire: ipc.WireEnvelope): Envelope = {
    val message: Message = try {
      // this can throw malformed json errors
      Json.fromJson[Message](Json.parse(wire.asString)).getOrElse(sys.error("Failure deserializing json."))
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
