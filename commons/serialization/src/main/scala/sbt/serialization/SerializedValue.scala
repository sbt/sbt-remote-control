package sbt.serialization

import org.json4s.JValue
import scala.pickling._
import scala.util.control.NonFatal
import scala.util.Try

/**
 * We serialize to and from this opaque type. The idea is to
 * hide exactly what we can serialize from/to and hide
 * which library we use to do it.
 */
sealed trait SerializedValue {
  def parse[T](implicit unpickler: SbtUnpickler[T]): Try[T]
}

object SerializedValue {
  def apply[V](value: V)(implicit pickler: SbtPickler[V]): SerializedValue =
    JsonValue[V](value)(pickler)
}

private[sbt] sealed trait SbtPrivateSerializedValue extends SerializedValue {
  def toJson: JsonValue
}

/** A value we have serialized as JSON */
private[sbt] final case class JsonValue(json: JValue) extends SbtPrivateSerializedValue {
  override def parse[T](implicit unpicklerForT: SbtUnpickler[T]): Try[T] = {
    implicit val u = unpicklerForT.underlying
    import sbt.pickling.json.pickleFormat
    import sbt.pickling.json.JSONPickle
    import org.json4s.native.JsonMethods._
    // TODO don't convert the AST to a string before parsing it!
    Try { JSONPickle(compact(render(json))).unpickle[T] }
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
  def apply[T](t: T)(implicit picklerForT: SbtPickler[T]): JsonValue = {
    implicit val pickler1: SPickler[T] = picklerForT.underlying
    import sbt.pickling.json.pickleFormat
    // TODO don't stringify the AST and then re-parse it!
    parseJson(t.pickle.value).get
  }

  val emptyObject = JsonValue(org.json4s.JObject(Nil))
}

/**
 * A value we have the info available to serialize, but we haven't
 *  picked a format yet. Allows us to defer format selection.
 * TODO this confuses pickling because V is an AnyRef.
 */
/*
private[sbt] final case class LazyValue[V](value: V, pickler: SbtPickler[V]) extends SbtPrivateSerializedValue {
  // this could theoretically avoid the round-trip in some cases, but
  // pretty annoying to figure out what those cases are so forget
  // it. Not expecting to actually call this really anyway because
  // we use LazyValue on the "send" side.
  override def parse[T](implicit unpickler: SbtUnpickler[T]): Option[T] =
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