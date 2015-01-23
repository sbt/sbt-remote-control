package sbt.serialization

import org.json4s.JValue
import org.json4s.JsonAST._
import scala.pickling._
import scala.util.control.NonFatal
import scala.util.Try
// Needed for pickle/unpickle methods.
import scala.pickling.functions._
import sbt.serialization.json.{
  JSONPickle
}

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

  /** Reconstitutes a SerialziedValue from a json string. */
  def fromJsonString(value: String): SerializedValue =
    JsonValue.fromJsonString(value)
  /** Reconstitutes a SerialziedValue from a json AST. */
  def fromJsonAST(value: JValue): SerializedValue =
    JsonValue.fromJValue(value)

}

// TODO - If this is meant to handle any kind of pickle format, it needs
//        to encode the pickle format directly.
//        i.e. it'd be nice if we had some kind of class that had a PickleFormat and Pickle
//        if we ever wanted to support alternative SerializedValues.
private[sbt] sealed trait SbtPrivateSerializedValue extends SerializedValue {
  /**
   * Returns this serialized value pickled into a json Tree.
   *
   * Note: This may be an expensive operation.
   */
  def toJson: JsonValue
  /**
   * Returns the serialzied value as a raw JSON string.
   *
   * Note: this may be an expensive operation.
   */
  def toJsonString: String
}

import sbt.serialization.json.JsonMethods._
/** A value we have serialized as JSON */
private[sbt] final case class JsonValue(pickledValue: JSONPickle) extends SbtPrivateSerializedValue {
  import sbt.serialization.json.pickleFormat
  override def parse[T](implicit unpicklerForT: Unpickler[T]): Try[T] =
    Try { unpickle[T](pickledValue) }

  override def toJson = this
  override def toJsonString: String = pickledValue.value
  override def equals(other: Any): Boolean =
    other match {
      case JsonValue(pv) => pickledValue == pv
      case _ => false
    }
  override def toString = toJsonString
}

private[sbt] object JsonValue {

  private[sbt] def fromJsonString(s: String): JsonValue =
    new JsonValue(JSONPickle(s))

  private[sbt] def fromJValue(jv: JValue): JsonValue =
    new JsonValue(JSONPickle.fromJValue(jv))

  // this is sort of dangerous because if you call it on a String
  // expecting to get the parseJson scenario above, you won't
  // get that, you'll get the JSON string pickled into JSON.
  def apply[T](t: T)(implicit picklerForT: SPickler[T]): JsonValue = {
    import sbt.serialization.json.pickleFormat
    // TODO don't stringify the AST and then re-parse it!
    JsonValue(pickle(t))
  }

  val emptyObject = JsonValue.fromJValue(org.json4s.JObject(Nil))
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
