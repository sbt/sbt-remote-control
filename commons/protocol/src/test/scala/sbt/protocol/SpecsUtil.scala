package sbt.protocol.spec

import sbt.protocol.Message
import scala.pickling.PickleOps, sbt.serialization._, sbt.serialization.json._
import scala.pickling.static._
import scala.pickling.ops._

object SpecsUtil {
  import sbt.serialization.spec.JUnitUtil._

  // TODO get rid of pickleMessage and parseMessage once pickle/unpickle are not macros
  def pickleMessage(m: Message): String =
    m.pickle.value

  def parseMessage(s: String): Message =
    s.unpickle[Message]
}
