package sbt.protocol

import sbt.impl.ipc
import java.io.File
import language.existentials
import scala.util.control.NonFatal
import sbt.serialization._

private[sbt] final case class Envelope(override val serial: Long, override val replyTo: Long, override val content: Message) extends ipc.Envelope[Message]

/**
 * This class is responsible for extracting from the wire protocol into
 *  the "class" protocol.  This may disappear at some point, as the duplication with ipc.Envelope may not be necessary.
 */
private[sbt] object Envelope {
  def apply(wire: ipc.WireEnvelope): Envelope = {
    //System.err.println(s"Attempting to parse ${wire.asString}")
    val message: Message =
      JsonValue.parseJson(wire.asString).flatMap(_.parse[Message]).recover({
        case NonFatal(e) =>
          try System.err.println(s"Failed to parse message ${wire.asString}: ${e.getClass.getName}: ${e.getMessage}") catch { case _: Throwable => }
          ErrorResponse(s"exception parsing json: ${e.getMessage}")
      }).get
    //System.err.println(s"Parsed it as $message")

    new Envelope(wire.serial, wire.replyTo, message)
  }
}
