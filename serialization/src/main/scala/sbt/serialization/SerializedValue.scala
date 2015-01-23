package sbt.serialization

import org.json4s.JValue
import org.json4s.JsonAST._
import scala.pickling.PicklingException
import scala.util.control.NonFatal
import scala.util.Try
// Needed for pickle/unpickle methods.
import scala.pickling.functions._

/**
 * We serialize to and from this opaque type. The idea is to
 * hide exactly what we can serialize from/to and hide
 * which library we use to do it.
 */
sealed trait SerializedValue {
  def parse[T](implicit unpickler: Unpickler[T]): Try[T]
}

object SerializedValue {
  def apply[V](value: V)(implicit pickler: SPickler[V]): SerializedValue =
    JsonValue[V](value)(pickler)
}

private[sbt] sealed trait SbtPrivateSerializedValue extends SerializedValue {
  def toJson: JsonValue
}

/** A value we have serialized as JSON */
private[sbt] final case class JsonValue(json: JValue) extends SbtPrivateSerializedValue {
  override def parse[T](implicit unpicklerForT: Unpickler[T]): Try[T] = {
    import sbt.serialization.json.pickleFormat
    import sbt.serialization.json.JSONPickle
    import org.json4s.native.JsonMethods._
    // TODO don't convert the AST to a string before parsing it!
    Try { unpickle[T](JSONPickle(compact(render(json)))) }
  }
  override def toJson = this

  def renderCompact: String = {
    import org.json4s.native.JsonMethods._
    compact(render(json))
  }

  def renderPretty: String = {
    import org.json4s.native.JsonMethods._
    pretty(render(json))
  }

  override def equals(other: Any): Boolean =
    other match {
      case JsonValue(oj) => jvalueEquals(json, oj)
      case _ => false
    }

  private def jvalueEquals(jvalue: JValue, jvalue2: JValue): Boolean =
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

  override def toString = renderCompact
}

private[sbt] object JsonValue {
  private def parseJValue(s: String): Try[JValue] = {
    jawn.support.json4s.Parser.parseFromString(s) recover {
      case e @ jawn.ParseException(msg, _, line, col) =>
        throw PicklingException(s"Parse error line $line column $col '$msg' in $s", Some(e))
      case e @ jawn.IncompleteParseException(msg) =>
        throw PicklingException(s"Incomplete json '$msg' in $s", Some(e))
    }
  }

  private[sbt] def parseJson(s: String): Try[JsonValue] =
    parseJValue(s) map { json => new JsonValue(json) }

  // this is sort of dangerous because if you call it on a String
  // expecting to get the parseJson scenario above, you won't
  // get that, you'll get the JSON string pickled into JSON.
  def apply[T](t: T)(implicit picklerForT: SPickler[T]): JsonValue = {
    import sbt.serialization.json.pickleFormat
    // TODO don't stringify the AST and then re-parse it!
    parseJson(pickle(t).value).get
  }

  val emptyObject = JsonValue(org.json4s.JObject(Nil))
}

/**
 * A value we have the info available to serialize, but we haven't
 *  picked a format yet. Allows us to defer format selection.
 * TODO this confuses pickling because V is an AnyRef.
 */
/*
private[sbt] final case class LazyValue[V](value: V, pickler: SPickler[V]) extends SbtPrivateSerializedValue {
  // this could theoretically avoid the round-trip in some cases, but
  // pretty annoying to figure out what those cases are so forget
  // it. Not expecting to actually call this really anyway because
  // we use LazyValue on the "send" side.
  override def parse[T](implicit unpickler: Unpickler[T]): Option[T] =
    toJson.parse[T]

  override def toJson = JsonValue(value)(pickler)
}

private[sbt] object LazyValue {
  // the macros can't generate a LazyValue pickler because it
  // contains a value of unknown (dynamic) type, but we can
  // create one by hand here that forces the LazyValue to be
  // evaluated to a JSON value.
  private val anyPickler = new SPickler[LazyValue[_]] with Unpickler[LazyValue[_]] {
    val jsonPickler = implicitly[SPickler[JsonValue]]
    val jsonUnpickler = implicitly[Unpickler[JsonValue]]
    override def pickle(lv: LazyValue[_], builder: PBuilder): Unit = {
      jsonPickler.pickle(lv.toJson, builder)
    }
    override def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      jsonUnpickler.unpickle(tag, preader)
    }
  }

  implicit def pickler[T]: SPickler[LazyValue[T]] =
    anyPickler.asInstanceOf[SPickler[LazyValue[T]]]

  implicit def unpickler[T]: Unpickler[LazyValue[T]] =
    anyPickler.asInstanceOf[Unpickler[LazyValue[T]]]
}
*/

// hypothetical to show why we have the SerializedValue hierarchy - not currently used
//private[sbt] final case class BinaryValue(bytes: Array[Byte]) extends SerializedValue
