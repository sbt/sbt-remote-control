package sbt.serialization

import org.json4s.{ JString, JValue }
import org.json4s.JsonAST._
import scala.pickling.PicklingException
import scala.util.control.NonFatal
import scala.util.Try
import scala.pickling.functions._
import sbt.serialization.json.{
  JSONPickle
}
import scala.pickling.{ FastTypeTag, PBuilder, PReader }

import sbt.serialization.json.JsonMethods._

/**
 * We serialize to and from this opaque type. The idea is to
 * hide exactly what we can serialize from/to and hide
 * which library we use to do it.
 *
 * What this will expose is the mechanism of using Pickler/Unpickler to
 * handle unknown serialized values.
 */
sealed trait SerializedValue {
  def parse[T](implicit unpickler: Unpickler[T]): Try[T]

  def toJsonString: String

  // TODO toBinary if/when we add binary pickling

  // private because we don't have our chosen
  // json AST library in the public API
  protected def toJValue: JValue

  final override def equals(other: Any): Boolean =
    other match {
      case null => false
      case sv: SerializedValue => toJValue.equals(sv.toJValue)
      case _ => false
    }

  final override def hashCode(): Int =
    toJValue.hashCode()
}

object SerializedValue {
  def apply[V](value: V)(implicit pickler: SPickler[V]): SerializedValue =
    LazyValue[V](value, pickler)

  /** Reconstitutes a SerializedValue from a json string. */
  def fromJsonString(value: String): SerializedValue =
    JsonValue(JSONPickle(value))

  // TODO fromBinary if/when we add binary pickling

  // this is in this file so it can use private JsonValue,
  // but the public implicit version is in a trait elsewhere
  private[sbt] object pickler extends SPickler[SerializedValue] with Unpickler[SerializedValue] {
    val cheaterTag = implicitly[FastTypeTag[JValue]]
    // TODO - This is super hacky mechanism to avoid issues w/ pinned types.
    override val tag = cheaterTag.asInstanceOf[FastTypeTag[SerializedValue]]
    def pickle(a: SerializedValue, builder: PBuilder): Unit = {
      val json = a.toJValue
      builder.hintTag(cheaterTag)
      builder.hintStaticallyElidedType()
      builder.beginEntry(json)
      builder.endEntry()
      //jsonPickler.pickle(spsv.toJson, builder)
    }
    def unpickle(tag: String, preader: PReader): Any = {
      preader.hintTag(cheaterTag)
      preader.hintStaticallyElidedType()
      preader.beginEntry()
      // TODO - Check beginEntry returns cheaterTag
      val value = preader.readPrimitive().asInstanceOf[JValue]
      preader.endEntry()
      JsonValue(JSONPickle.fromJValue(value))
    }
  }
}
/** A value we have serialized as JSON */
private final case class JsonValue(pickledValue: JSONPickle) extends SerializedValue {
  require(pickledValue ne null)
  require(pickledValue.parsedValue ne null)

  import sbt.serialization.json.pickleFormat
  override def parse[T](implicit unpicklerForT: Unpickler[T]): Try[T] =
    Try { unpickle[T](pickledValue) }

  override def toJsonString: String = pickledValue.value
  // this deliberately doesn't simply toJsonString because it would
  // be broken to use toString to get parseable json (since the SerializedValue
  // may not be a JsonValue)
  override def toString = s"JsonValue($toJsonString)"

  override def toJValue: JValue = pickledValue.parsedValue
}

private object JsonValue {

  def fromPicklee[T](t: T)(implicit picklerForT: SPickler[T]): JsonValue = {
    import sbt.serialization.json.pickleFormat
    JsonValue(pickle(t))
  }
}

/**
 * A value we have the info available to serialize, but we haven't
 *  picked a format yet. Allows us to defer format selection, or
 *  for in-process uses we could even theoretically skip serialization.
 */
private final case class LazyValue[V](value: V, pickler: SPickler[V]) extends SerializedValue {
  // this could theoretically avoid the round-trip through JSON
  // in some cases, but pretty annoying to figure out what those
  // cases are so forget it.
  // Not expecting to actually call this really anyway because
  // we use LazyValue on the "send" side.
  override def parse[T](implicit unpickler: Unpickler[T]): Try[T] =
    toJson.parse[T]

  override def toJsonString = toJson.toJsonString

  override def toJValue = toJson.toJValue

  private lazy val jsonValue = JsonValue.fromPicklee(value)(pickler)

  def toJson: JsonValue = jsonValue
}
