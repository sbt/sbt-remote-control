package sbt.serialization
import scala.pickling.{ SPickler, Unpickler, FastTypeTag, PBuilder, PReader }
import org.json4s.{ JValue, JString }

// TODO comment explaining FakeTags
private object FakeTags {
  val JValue = implicitly[FastTypeTag[JValue]]
}

trait SerializationPicklerUnpickler extends sbt.pickling.CustomPicklerUnpickler {
  // TODO move this to sbt.serialization once it works to do so
  private implicit def staticOnly = scala.pickling.static.StaticOnly

  private object jvaluePickler extends SPickler[JValue] with Unpickler[JValue] {
    val tag = implicitly[FastTypeTag[JValue]]
    val stringPickler = implicitly[SPickler[String]]
    val stringUnpickler = implicitly[Unpickler[String]]
    def pickle(jv: JValue, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FakeTags.JValue)
      builder.beginEntry(jv)
      builder.endEntry()
      builder.popHints()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      preader.pushHints()
      preader.hintTag(tag)
      preader.beginEntryNoTag()
      val result = preader.readPrimitive
      preader.endEntry
      preader.popHints()
      result
    }
  }

  implicit object jsonValuePickler extends SPickler[JsonValue] with Unpickler[JsonValue] {
    val tag = implicitly[FastTypeTag[JsonValue]]

    def pickle(jv: JsonValue, builder: PBuilder): Unit = {
      builder.pushHints()
      builder.hintTag(FakeTags.JValue)
      jvaluePickler.pickle(jv.json, builder)
      builder.popHints()
    }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      val json = jvaluePickler.unpickle(FakeTags.JValue, preader).asInstanceOf[JValue]
      JsonValue(json)
    }
  }

  implicit object serializedValuePickler extends SPickler[SerializedValue] with Unpickler[SerializedValue] {
    val tag = implicitly[FastTypeTag[SerializedValue]]

    private val jsonPickler = implicitly[SPickler[JsonValue]]
    private val jsonUnpickler = implicitly[Unpickler[JsonValue]]
    def pickle(a: SerializedValue, builder: PBuilder): Unit =
      a match {
        case spsv: SbtPrivateSerializedValue => jsonPickler.pickle(spsv.toJson, builder)
      }
    def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
      jsonUnpickler.unpickle(tag, preader)
    }
  }
}
