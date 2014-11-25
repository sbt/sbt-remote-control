package sbt.pickling.spec

import sbt.protocol.Message
import scala.pickling._, sbt.pickling.json._

object SpecsUtil {
  def parseMessage(s: String): Message =
    ??? // TODO // s.unpickle[Message]
}
