package sbt.pickling.spec

import sbt.protocol.Message

object SpecsUtil {
  def parseMessage(s: String): Message = {
    import scala.pickling._, sbt.pickling.json._
    s.unpickle[Message]
  }
}