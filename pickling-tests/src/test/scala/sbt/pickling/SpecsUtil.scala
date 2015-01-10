package sbt.pickling.spec

import sbt.protocol.Message

import scala.pickling.PickleOps, sbt.pickling._, sbt.pickling.json._, sbt.serialization._
import scala.pickling.static._

object SpecsUtil {
  import JUnitUtil._

  // TODO get rid of pickleMessage and parseMessage once pickle/unpickle are not macros
  def pickleMessage(m: Message): String =
    m.pickle.value

  def parseMessage(s: String): Message =
    s.unpickle[Message]
}
