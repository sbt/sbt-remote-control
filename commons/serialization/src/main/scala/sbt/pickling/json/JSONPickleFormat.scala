package sbt.pickling

import sbt.pickling._
import scala.pickling.{
  FastTypeTag,
  Output,
  PBuilder,
  PReader,
  Pickle,
  PickleFormat,
  PickleTools,
  PicklingException,
  SPickler,
  StringOutput,
  Unpickler,
  UnpickleOps
}
import scala.pickling.internal.lookupUnpicklee
// FIXME this isn't threadsafe right? we need to get rid of its use.
import scala.reflect.runtime.universe.{ Mirror, ClassSymbol, definitions }
import definitions._
import org.json4s._
import scala.util.parsing.json.JSONFormat.quoteString
import scala.collection.mutable.{ StringBuilder, Stack }
import scala.util.{ Success, Failure }

package json {

  import scala.pickling.Hints

  object `package` {
    implicit val pickleFormat: JSONPickleFormat = new JSONPickleFormat
    // TODO both of these are pretty sketchy probably?
    // import scala.language.implicitConversions
    //implicit def toJSONPickle(value: String): JSONPickle = JSONPickle(value)
    //implicit def toUnpickleOps(value: String): UnpickleOps = new UnpickleOps(JSONPickle(value))
  }

  case class JSONPickle(value: String) extends Pickle {
    type ValueType = String
    type PickleFormatType = JSONPickleFormat
  }

  class JSONPickleFormat extends PickleFormat {
    type PickleType = JSONPickle
    type OutputType = Output[String]
    def createBuilder() = new VerifyingJSONPickleBuilder(this, new StringOutput)
    def createBuilder(out: Output[String]): PBuilder = new VerifyingJSONPickleBuilder(this, out)
    def createReader(pickle: JSONPickle, mirror: Mirror) = {
      jawn.support.json4s.Parser.parseFromString(pickle.value) match {
        case Success(json) => new JSONPickleReader(json, mirror, this)
        case Failure(e) => throw new PicklingException("failed to parse \"" + pickle.value + "\" as JSON: " + e.getMessage)
      }
    }
  }
  object JSONPickleFormat {
    private[json] val TYPE_TAG_FIELD = "$type"
    private[json] val DYNAMIC_KEY_FIELD = "$keys"
    private[json] val REF_ID_FIELD = "$ref"

    private[json] def isSpecialField(name: String): Boolean =
      (TYPE_TAG_FIELD == name) || (DYNAMIC_KEY_FIELD == name) || (REF_ID_FIELD == name)
    private[json] def isElidedField(name: String): Boolean =
      (DYNAMIC_KEY_FIELD == name)
  }

  sealed trait BuilderState {
    def previous: BuilderState
  }
  case class CollectionState(val previous: BuilderState, numElements: Int, hasInput: Boolean) extends BuilderState
  case class RawEntryState(previous: BuilderState, picklee: Any, hints: Hints, var wasCollectionOrMap: Boolean = false) extends BuilderState
  case class MapEntryState(val previous: BuilderState, picklee: Any, hints: Hints) extends BuilderState
  case class RefEntryState(val previous: BuilderState) extends BuilderState
  object EmptyState extends BuilderState {
    def previous = this
  }

  // A slow implementation of of a pickle builder
  // This uses a TON of branch statements to ensure the builder is in the correct state for any call
  // and to programatically enforce constraints of SPickler implementations.
  // We use this just to verify our own picklers.
  class VerifyingJSONPickleBuilder(format: JSONPickleFormat, buf: Output[String]) extends PBuilder with PickleTools {
    import JSONPickleFormat._
    var state: BuilderState = EmptyState
    //(tag.key startsWith "scala.Option[")
    private def isJValue(tag: FastTypeTag[_]): Boolean =
      (tag.key startsWith "org.json4s.JsonAST.")
    // Here we get notified of object/value-like things.
    override def beginEntry(picklee: Any): PBuilder = withHints { hints =>
      // Here we check to see if we need to serialize a reference.  These are used to avoid circular object
      // dependencies for picklers which have circluarly-references objects.
      if (hints.oid != -1) {
        buf.put("{\"" + REF_ID_FIELD + "\":" + hints.oid + "}")
        state = RefEntryState(state)
      } else {
        state = new RawEntryState(state, picklee, hints)
      }
      this
    }
    override def putField(name: String, pickler: (PBuilder) => Unit): PBuilder =
      if (!isElidedField(name)) {
        state match {
          case x: RawEntryState =>
            x.wasCollectionOrMap = true
            // Now we know we're in a map state, so we swap into map state.
            state = MapEntryState(x.previous, x.picklee, x.hints)
            buf.put("{")
          case _: MapEntryState =>
            // here we just need another ,
            buf.put(",")
          case _ => sys.error("Cannot put a field when not in entry state!")
        }
        // Here we must append all the stringy things around the field.
        buf.put('"' + name + "\":")
        pickler(this)
        this
      } else this
    override def endEntry(): Unit = {
      state match {
        case RawEntryState(prev, _, _, true) =>
          // Here we do nothing because it was a collection.
          state = prev
        case RawEntryState(prev, picklee, hints, false) =>
          // Here we have to actually serialize the thing, as we're not a collection or a map.
          if (primitives.contains(hints.tag.key))
            primitives(hints.tag.key)(picklee)
          else if (primitiveArrays.contains(hints.tag.key)) {
            primitiveArrays(hints.tag.key)(picklee)
          } else if (isJValue(hints.tag)) {
            // TODO - Serialize it.
            import org.json4s.native.JsonMethods._
            buf.put(compact(render(picklee.asInstanceOf[JValue])))
          } else {
            // Note: It's possible the object is empty, so we just put an empty object here,
            // as the type we're serializing may not have any contents.
            // we also serialize the "$type" here if needed.
            // Ignore isStaticallyElidedType. Always output $type.
            buf.put("{")
            appendTagString(picklee, hints)
            buf.put("}")
          }
          state = prev
        case MapEntryState(prev, picklee, hints) =>
          // Ignore isStaticallyElidedType and always send the $type down.
          buf.put(",")
          appendTagString(picklee, hints)
          buf.put("}")
          state = prev
        case RefEntryState(prev) =>
          state = prev
        case _ => sys.error("Unable to endEntry() when not in entry state!")
      }
    }
    private def appendTagString(picklee: Any, hints: Hints): Unit =
      buf.put("\"" + TYPE_TAG_FIELD + "\":\"" + makeTagString(picklee, hints) + "\"")
    private def makeTagString(picklee: Any, hints: Hints): String =
      if (hints.tag.key.contains("anonfun$")) picklee.getClass.getName
      else hints.tag.key

    // We cover ararys of primitives separately here.
    // NOTE: these are special cased in the core pickler design (probably for binary encoding efficiency)
    private val primitiveArrays = Map[String, Any => Unit](
      FastTypeTag.ArrayByte.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Byte]], FastTypeTag.Byte)),
      FastTypeTag.ArrayShort.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Short]], FastTypeTag.Short)),
      FastTypeTag.ArrayChar.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Char]], FastTypeTag.Char)),
      FastTypeTag.ArrayInt.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Int]], FastTypeTag.Int)),
      FastTypeTag.ArrayLong.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Long]], FastTypeTag.Long)),
      FastTypeTag.ArrayBoolean.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Boolean]], FastTypeTag.Boolean)),
      FastTypeTag.ArrayFloat.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Float]], FastTypeTag.Float)),
      FastTypeTag.ArrayDouble.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Double]], FastTypeTag.Double)))
    private def pickleArray(arr: Array[_], tag: FastTypeTag[_]) = {
      beginCollection(arr.length)
      pushHints()
      hintStaticallyElidedType()
      hintTag(tag)
      pinHints()
      var i = 0
      while (i < arr.length) {
        putElement(b => b.beginEntry(arr(i)).endEntry())
        i += 1
      }
      popHints()
      endCollection()
    }

    private val primitives = Map[String, Any => Unit](
      FastTypeTag.Unit.key -> ((picklee: Any) => buf.put("\"()\"")),
      FastTypeTag.Null.key -> ((picklee: Any) => buf.put("null")),
      FastTypeTag.Ref.key -> ((picklee: Any) => throw new Error("fatal error: shouldn't be invoked explicitly")),
      FastTypeTag.Int.key -> ((picklee: Any) => buf.put(picklee.toString)),
      FastTypeTag.Long.key -> ((picklee: Any) => buf.put(picklee.toString)),
      FastTypeTag.Short.key -> ((picklee: Any) => buf.put(picklee.toString)),
      FastTypeTag.Double.key -> ((picklee: Any) => buf.put(picklee.toString)),
      FastTypeTag.Float.key -> ((picklee: Any) => buf.put(picklee.toString)),
      FastTypeTag.Boolean.key -> ((picklee: Any) => buf.put(picklee.toString)),
      FastTypeTag.Byte.key -> ((picklee: Any) => buf.put(picklee.toString)),
      FastTypeTag.Char.key -> ((picklee: Any) => buf.put("\"" + quoteString(picklee.toString) + "\"")),
      FastTypeTag.String.key -> ((picklee: Any) => buf.put("\"" + quoteString(picklee.toString) + "\"")) // Note we've removed all Array knowledge in favor of traeting this NOT as primitive types, but instead
      // provide a collection pickler for them.
      )

    override def beginCollection(length: Int): PBuilder = {
      state match {
        case x: RawEntryState =>
          x.wasCollectionOrMap = true
          state = CollectionState(x, length, false)
          buf.put("[")
          this
        case _ => sys.error(s"Unable to begin collection when in unknown state: $state")
      }
    }
    override def putElement(pickler: (PBuilder) => Unit): PBuilder =
      state match {
        case s: CollectionState =>
          // TODO - Verify
          if (s.hasInput) { buf.put(",") } else {
            state = s.copy(hasInput = true)
          }
          pickler(this)
          this
        case _ => sys.error("Cannot put an element without first specifying a collection.")
      }
    override def endCollection(): Unit =
      state match {
        case s: CollectionState =>
          buf.put("]")
          state = s.previous
        case _ => sys.error("cannot end a collection when not in collection state!")
      }

    override def result(): JSONPickle = {
      // TODO - verify everything is done, and we have no state stack...
      if (state != EmptyState) sys.error("Failed to close/end all entries and collections!")
      JSONPickle(buf.toString)
    }
  }

  class JSONPickleReader(var datum: Any, val mirror: Mirror, format: JSONPickleFormat) extends PReader with PickleTools {
    import JSONPickleFormat._
    private var lastReadTag: FastTypeTag[_] = null
    private val primitives = Map[String, () => Any](
      FastTypeTag.Unit.key -> (() => ()),
      FastTypeTag.Null.key -> (() => null),
      FastTypeTag.Ref.key -> (() => lookupUnpicklee(datum match {
        case obj: JObject =>
          (obj \ REF_ID_FIELD) match {
            case JDouble(num) => num.toInt
            case x: JValue => unexpectedValue(x)
          }
      })),
      FastTypeTag.Int.key -> (() => datum match {
        case JDouble(num) => num.toInt
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.Short.key -> (() => datum match {
        case JDouble(num) => num.toShort
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.Double.key -> (() => datum match {
        case JDouble(num) => num
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.Float.key -> (() => datum match {
        case JDouble(num) => num.toFloat
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.Long.key -> (() => datum match {
        case JDouble(num) => num.toLong
        case JString(s) => s.toLong
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.Byte.key -> (() => datum match {
        case JDouble(num) => num.toByte
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.Boolean.key -> (() => datum match {
        case JBool(b) => b
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.Char.key -> (() => datum match {
        case JString(s) => s.head
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.String.key -> (() => datum match {
        case JString(s) => s
        case x: JValue => unexpectedValue(x)
      }),
      FastTypeTag.ArrayByte.key -> (() => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toByte
            case x: JValue => unexpectedValue(x)
          }
      }).toArray),
      FastTypeTag.ArrayShort.key -> (() => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toShort
            case x: JValue => unexpectedValue(x)
          }
      }).toArray),
      FastTypeTag.ArrayChar.key -> (() => (datum match {
        case JArray(arr) =>
          arr map {
            case JString(s) => s.head
            case x: JValue => unexpectedValue(x)
          }
      }).toArray),
      FastTypeTag.ArrayInt.key -> { () =>
        (datum match {
          case JArray(arr) =>
            arr map {
              case JDouble(num) => num.toInt
              case x: JValue => unexpectedValue(x)
            }
        }).toArray
      },
      FastTypeTag.ArrayLong.key -> (() => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toLong
            case JString(s) => s.toLong
            case x: JValue => unexpectedValue(x)
          }
      }).toArray),
      FastTypeTag.ArrayBoolean.key -> (() => (datum match {
        case JArray(arr) =>
          arr map {
            case JBool(b) => b
            case x: JValue => unexpectedValue(x)
          }
      }).toArray),
      FastTypeTag.ArrayFloat.key -> (() => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toFloat
            case x: JValue => unexpectedValue(x)
          }
      }).toArray),
      FastTypeTag.ArrayDouble.key -> (() => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num
            case x: JValue => unexpectedValue(x)
          }
      }).toArray))
    private def unexpectedValue(value: JValue): Nothing =
      throw new PicklingException("unexpected value: " + value.toString)
    private def mkNestedReader(datum: Any) = {
      val nested = new JSONPickleReader(datum, mirror, format)
      if (this.areHintsPinned) {
        nested.pinHints()
        nested.hints = hints
        nested.lastReadTag = lastReadTag
      }
      nested
    }

    def beginEntryNoTag(): String = beginEntryNoTagDebug(true)
    def beginEntryNoTagDebug(debugOn: Boolean): String =
      if (debugOn) {
        val tag = beginEntry()
        if (tag == null) sys.error("tag is null")
        tag.key
      } else beginEntry().key
    def beginEntry(): FastTypeTag[_] = withHints { hints =>
      lastReadTag = {
        if (datum == null) FastTypeTag.Null
        else {
          datum match {
            case obj: JObject if (obj findField {
              case JField(REF_ID_FIELD, _) => true
              case _ => false
            }).isDefined => FastTypeTag.Ref
            case obj: JObject if (obj findField {
              case JField(TYPE_TAG_FIELD, _) => true
              case _ => false
            }).isDefined =>
              (obj \ TYPE_TAG_FIELD) match {
                case JString(s) =>
                  try {
                    val tagFromJson = FastTypeTag(mirror, s)
                    // Given sealed trait Fruit that has Apple and Orange as child type,
                    // a) Choose Apple if json says Apple and hint says Fruit
                    // b) Choose Orange if json says Apple and hint says Orange
                    // c) Choose Apple if json has unknown and hint says Apple
                    if (ManifestUtil.isApproxSubType(tagFromJson, hints.tag)) tagFromJson
                    else hints.tag
                  } catch {
                    case e: Throwable if e.getMessage contains "cannot find class" =>
                      if (Option(hints.tag.tpe.typeSymbol) map {
                        case sym: ClassSymbol => sym.isAbstractClass || sym.isTrait
                        case _ => true
                      } getOrElse (true)) throw PicklingException(e.getMessage)
                      else hints.tag
                    case e: Throwable => throw e
                  }
                case x => sys.error("Unexpected field: " + x.toString)
              }
            case _ => hints.tag
          }
        }
      }
      lastReadTag
    }
    def atPrimitive: Boolean = primitives.contains(lastReadTag.key)
    private val primitiveSeqKeys: Vector[String] = Vector(
      FastTypeTag.ArrayByte.key, FastTypeTag.ArrayShort.key, FastTypeTag.ArrayChar.key,
      FastTypeTag.ArrayInt.key, FastTypeTag.ArrayLong.key, FastTypeTag.ArrayBoolean.key,
      FastTypeTag.ArrayFloat.key, FastTypeTag.ArrayDouble.key)
    def atJValue: Boolean = (lastReadTag.key startsWith "org.json4s.JsonAST.")
    def readJValue: JValue =
      datum match {
        case x: JValue => x
      }

    def readPrimitive(): Any = {
      datum match {
        case x if atJValue =>
          readJValue
        case JArray(list) if !primitiveSeqKeys.contains(lastReadTag.key) =>
          // now this is a hack!
          val value = mkNestedReader(list.head).primitives(lastReadTag.key)()
          datum = JArray(list.tail)
          value
        case obj: JObject if lastReadTag.key != FastTypeTag.Ref.key =>
          mkNestedReader(obj \ "$value").primitives.get(lastReadTag.key)
            .getOrElse(throw new PicklingException(s"$lastReadTag cannot be read as a primitive from $obj"))()
        case x if atPrimitive =>
          primitives(lastReadTag.key)()
        case JString(s) =>
          s
        case other =>
          throw new PicklingException(s"""$lastReadTag cannot be read as a primitive from $other""")
      }
    }
    def atObject: Boolean = datum.isInstanceOf[JObject]
    def readField(name: String): JSONPickleReader = {
      datum match {
        // Handle the  magic "keys" field.
        case obj: JObject if DYNAMIC_KEY_FIELD == name =>
          mkNestedReader(JArray(obj.values.keys.filterNot(isSpecialField).map(k => JString(k)).toList))
        case obj: JObject => mkNestedReader(obj \ name)
      }
    }
    def endEntry(): Unit = {}
    def beginCollection(): PReader = this // readField("$elems")
    // support readLength of non arrays for Option[A] to pretend to be a collection
    def readLength(): Int = {
      datum match {
        case JNothing => 0 // Option[A] support
        case JNull => 0 // Option[A] support
        case JArray(list) => list.length
        case _ => 1 // Option[A] support
      }
    }
    private var i = 0
    def readElement(): PReader = {
      val reader = {
        datum match {
          case JArray(list) => mkNestedReader(list(i))
          case _ => this // Option[A] support
        }
      }
      i += 1
      reader
    }
    def endCollection(): Unit = {}
  }
}
