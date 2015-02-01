package sbt.serialization

import org.json4s.{ JString, JValue }
import org.json4s.JsonAST._
import scala.pickling.PicklingException
import scala.util.control.NonFatal
import scala.util.{ Try, Success }
import scala.pickling.functions._
import sbt.serialization.json.{
  JSONPickle
}
import sbt.serialization.json.JsonMethods._
import sbt.serialization.json.JSONPickleFormat

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

  /**
   * Returns true if the supplied unpickler matches the
   * type tag in the SerializedValue. This can return
   * false in some cases where parse[T] could succeed (parse
   * is allowed to use duck typing). So this should only
   * be used when the type to be unpickled is not known
   * for sure and a discriminator is required. The advantages
   * of hasTag over simply attempting to parse are that it
   * doesn't throw an expensive exception on match failure,
   * and it can't accidentally match a structurally-compatible
   * but distinct type.
   */
  def hasTag[T](implicit unpickler: Unpickler[T]): Boolean

  def toJsonString: String

  // TODO toBinary if/when we add binary pickling

  // private because we don't have our chosen
  // json AST library in the public API
  protected def toJValue: JValue

  final override def equals(other: Any): Boolean =
    other match {
      case null => false
      case sv: SerializedValue => json.JsonMethods.jvalueEquals(toJValue, sv.toJValue)
      case _ => false
    }

  final override def hashCode(): Int =
    json.JsonMethods.jvalueHashCode(toJValue)
}

object SerializedValue {
  def apply[V](value: V)(implicit pickler: SPickler[V]): SerializedValue =
    LazyValue[V](value, pickler)

  /** Reconstitutes a SerializedValue from a json string. */
  def fromJsonString(value: String): SerializedValue =
    JsonValue(JSONPickle(value))

  // TODO fromBinary if/when we add binary pickling

  // this is in this file so it can use private JsonValue,
  // but the public implicit version is in a trait elsewhere.
  // NOTE: this pickler ONLY works with our JSONPickleFormat because
  // it assumes JValue is a "primitive" known to the format.
  // we can adjust this if we add a binary format.
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

  def hasTag[T](implicit unpickler: Unpickler[T]): Boolean =
    pickledValue.readTypeTag.map(tag => tag == unpickler.tag.key).getOrElse(false)

  override def toJsonString: String = pickledValue.value
  // this deliberately doesn't simply toJsonString because it would
  // be broken to use toString to get parseable json (since the SerializedValue
  // may not be a JsonValue)
  override def toString = s"JsonValue($toJsonString)"

  override def toJValue: JValue = pickledValue.parsedValue
}

/**
 * A value we have the info available to serialize, but we haven't
 *  picked a format yet. Allows us to defer format selection, or
 *  for in-process uses we can even try to skip serialization.
 */
private final case class LazyValue[V](value: V, pickler: SPickler[V]) extends SerializedValue {
  // we use this to optimize a common case
  private def unpicklerMatchesExactly[T](unpickler: Unpickler[T]): Boolean = {
    // We compare tag.key because FastTypeTag.equals uses Mirror
    // and Type and we don't want to use the reflection API.
    pickler.tag.key == unpickler.tag.key
  }

  // this could theoretically avoid the round-trip through JSON
  // in some cases, but pretty annoying to figure out what those
  // cases are so forget it.
  // Not expecting to actually call this really anyway because
  // we use LazyValue on the "send" side.
  override def parse[T](implicit unpickler: Unpickler[T]): Try[T] =
    if (unpicklerMatchesExactly(unpickler)) Success(value.asInstanceOf[T])
    // this allows duck typing to succeed and also handles
    // V=Fruit, T=Apple case.
    else toJson.parse[T]

  def hasTag[T](implicit unpickler: Unpickler[T]): Boolean =
    // the toJson is needed if you have a Fruit pickler
    // and an Apple unpickler, so $type is Apple but pickler.tag
    // is Fruit
    unpicklerMatchesExactly(unpickler) || toJson.hasTag[T]

  override def toJsonString = toJson.toJsonString

  override def toJValue = toJson.toJValue

  private lazy val jsonValue =
    JsonValue(pickle(value)(sbt.serialization.json.pickleFormat, pickler))

  def toJson: JsonValue = jsonValue
}
