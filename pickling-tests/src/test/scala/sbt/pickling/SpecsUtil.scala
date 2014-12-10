package sbt.pickling.spec

import sbt.protocol.Message

import scala.pickling._, sbt.pickling.json._
// TODO this should not break!
//import scala.pickling.static._

object SpecsUtil {
  def pickleMessage(m: Message): String =
    m.pickle.value

  def parseMessage(s: String): Message =
    s.unpickle[Message]
}
