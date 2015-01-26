package sbt.serialization
package pickler

import scala.pickling.{ FastTypeTag, PBuilder, PReader }
import org.json4s.{ JValue, JString }

// TODO comment explaining FakeTags
private object FakeTags {
  val JValue = implicitly[FastTypeTag[JValue]]
}

trait SerializationPicklers {
  // TODO - this shoudl only be available if the json format is implciitly available...
  //  ALso this can probably just extend Primitive Pcikler
  private object jvaluePickler extends SPickler[JValue] with Unpickler[JValue] {
    val tag = implicitly[FastTypeTag[JValue]]
    def pickle(jv: JValue, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FakeTags.JValue)
      builder.beginEntry(jv)
      builder.endEntry()
      builder.popHints()
    }
    def unpickle(tag: String, preader: PReader): Any = {
      val result = preader.readPrimitive
      result
    }
  }

  implicit object jsonValuePickler extends SPickler[JsonValue] with Unpickler[JsonValue] {
    val tag = implicitly[FastTypeTag[JsonValue]]

    def pickle(jv: JsonValue, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FakeTags.JValue)
      // TODO - Ideally we could just serialize the JSON string DIRECTLY here.
      jvaluePickler.pickle(jv.pickledValue.parsedValue, builder)
      builder.popHints()
    }
    def unpickle(tag: String, preader: PReader): Any = {
      // TODO - we need to tag this, most likely.
      val json = jvaluePickler.unpickle(FakeTags.JValue.key, preader).asInstanceOf[JValue]
      JsonValue.fromJValue(json)
    }
  }

  implicit object serializedValuePickler extends SPickler[SerializedValue] with Unpickler[SerializedValue] {
    val cheaterTag = implicitly[FastTypeTag[JValue]]
    // TODO - This is super hacky mechanism to avoid issues w/ pinned types.
    override val tag = cheaterTag.asInstanceOf[FastTypeTag[SerializedValue]]
    def pickle(a: SerializedValue, builder: PBuilder): Unit =
      a match {
        case spsv: SbtPrivateSerializedValue =>
          val json = spsv.toJson.pickledValue.parsedValue
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
      JsonValue.fromJValue(value)
    }
  }
}
