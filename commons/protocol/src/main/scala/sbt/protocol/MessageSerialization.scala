package sbt.protocol

import sbt.serialization._
import scala.pickling.{ SPickler, Unpickler }

/**
 * TODO get rid of this object, it just holds one leftover
 * method we probably don't need
 */
private[protocol] object MessageSerialization {
  // this makes it prettier when writing json by hand e.g. in JavaScript
  private def removeDollar(s: String) = {
    val i = s.lastIndexOf('$')
    if (i >= 0)
      s.substring(0, i)
    else
      s
  }
  // avoiding class.getSimpleName because apparently it's buggy with some
  // Scala name manglings
  private def lastChunk(s: String) = {
    val i = s.lastIndexOf('.')
    if (i >= 0)
      s.substring(i + 1)
    else
      s
  }
  private[protocol] def makeSimpleName(klass: Class[_]): String =
    removeDollar(lastChunk(klass.getName))
}
