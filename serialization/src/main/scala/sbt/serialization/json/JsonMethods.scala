package sbt.serialization.json

import org.json4s.{
  JsonInput,
  StringInput,
  StreamInput,
  ReaderInput,
  FileInput,
  JValue,
  JField,
  JNothing,
  JBool,
  JString,
  JInt,
  JDecimal,
  JArray,
  JObject,
  JNull,
  JDouble
}
import java.io.File
import scala.pickling.PicklingException
import scala.util.Try

/** An implementation of JsonMethods for json4s that uses Jawn and our own toStrings. */
object JsonMethods extends org.json4s.JsonMethods[JValue] {
  // Redner doesn't do anything, as we aren't translating to an intermediate format before rendering.
  override def render(value: JValue): JValue = value
  // TODO - Write this.
  override def pretty(d: JValue): String = compact(d)
  // Compact rendering.
  override def compact(d: JValue): String = {
    val buf = new StringBuilder("")
    import org.json4s._
    def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
    def trimObj(xs: List[JField]) = xs.filter(_._2 != JNothing)
    def append(d: JValue): Unit = {
      d match {
        case null => buf.append("null")
        case JBool(true) => buf.append("true")
        case JBool(false) => buf.append("false")
        case JDouble(n) => buf.append(n.toString)
        case JDecimal(n) => buf.append(n.toString)
        case JInt(n) => buf.append(n.toString)
        case JNull => buf.append("null")
        // TODO - better error message
        case JNothing => sys.error("can't render 'nothing'")
        // TODO - does this even make sense?
        case JString(null) => buf.append("null")
        case JString(s) =>
          buf.append("\"")
          buf.append(ParserUtil.quote(s))
          buf.append("\"")
        case JArray(arr) =>
          buf.append("[")
          val trimmed = trimArr(arr)
          var l = trimmed
          while (!l.isEmpty) {
            val el = l.head
            if (l ne trimmed) buf.append(",")
            append(el)
            l = l.tail
          }
          buf.append("]")
        case JObject(obj) =>
          buf.append("{")
          val trimmed = trimObj(obj)
          var l = trimmed
          while (!l.isEmpty) {
            val (k, v) = l.head
            if (l ne trimmed) buf.append(",")
            buf.append("\"").append(ParserUtil.quote(k)).append("\":")
            append(v)
            l = l.tail
          }
          buf.append("}")
      }
    }
    append(d)
    buf.toString
  }

  override def parse(in: JsonInput, useBigDecimalForDouble: Boolean): JValue =
    parseTry(in, useBigDecimalForDouble).get
  override def parseOpt(in: JsonInput, useBigDecimalForDouble: Boolean): Option[JValue] =
    parseTry(in, useBigDecimalForDouble).toOption
  def parseTry(in: JsonInput, useBigDecimalForDouble: Boolean): Try[JValue] = {
    val result: Try[JValue] = in match {
      case StringInput(string) => jawn.support.json4s.Parser.parseFromString(string)
      // TODO - We should support the reader case too.
      case ReaderInput(reader) => util.Try(???)
      case StreamInput(stream) =>
        val in = java.nio.channels.Channels.newChannel(stream)
        try jawn.support.json4s.Parser.parseFromChannel(in)
        finally in.close()
      case FileInput(file: File) =>
        val in = (new java.io.FileInputStream(file)).getChannel
        try jawn.support.json4s.Parser.parseFromChannel(in)
        finally in.close()
    }
    result recover {
      case e @ jawn.ParseException(msg, _, line, col) =>
        throw PicklingException(s"Parse error line $line column $col '$msg' in $in", Some(e))
      case e @ jawn.IncompleteParseException(msg) =>
        throw PicklingException(s"Incomplete json '$msg' in $in", Some(e))
    }
  }

  final def jvalueEquals(jvalue: JValue, jvalue2: JValue): Boolean =
    (jvalue, jvalue2) match {
      case (JNull, null) | (null, JNull) | (JNull, JNull) | (null, null) => true
      case (JNothing, JNothing) => true
      case (JBool(v), JBool(v2)) => v == v2
      case (JDouble(v), JDouble(v2)) => v == v2
      case (JString(v), JString(v2)) => v == v2
      case (JArray(el), JArray(el2)) => (el.size == el2.size) && (el.zip(el2).forall((jvalueEquals _).tupled))
      case (JObject(el), JObject(el2)) =>
        (el.size == el2.size) && (
          el.sortBy(_._1).zip(el2.sortBy(_._1)).forall {
            case ((k, v), (k2, v2)) => (k == k2) && jvalueEquals(v, v2)
          })
      case (left, right) =>
        System.err.println("Found differences between [$left] and [$right]")
        false
    }
}