package sbt.protocol

import sbt.impl.ipc
import java.io.File
import language.existentials

private[sbt] final case class Envelope(override val serial: Long, override val replyTo: Long, override val content: Message) extends ipc.Envelope[Message]

/**
 * This class is responsible for extracting from the wire protocol into
 *  the "class" protocol.  This may disappear at some point, as the duplication with ipc.Envelope may not be necessary.
 */
private[sbt] object Envelope {
  def apply(wire: ipc.WireEnvelope): Envelope = {
    val message: Message = // FIXME
      Some((???).asInstanceOf[Message]).getOrElse(ErrorResponse("exception parsing json"))

    new Envelope(wire.serial, wire.replyTo, message)
  }

}
