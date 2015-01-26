package sbt.serialization

import scala.pickling.{
  FastTypeTag,
  Output,
  PBuilder,
  PReader,
  Pickle,
  PickleFormat,
  PickleTools,
  PicklingException,
  StringOutput,
  UnpickleOps
}
import scala.pickling.internal.lookupUnpicklee
// FIXME this isn't threadsafe right? we need to get rid of its use.
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

  sealed abstract class JSONPickle extends Pickle {
    type ValueType = String
    type PickleFormatType = JSONPickleFormat
    //abstract val value: String
    /** The value in the pickled parsed into a JValue AST.  note this may throw. */
    def parsedValue: JValue

    override final def equals(other: Any): Boolean = other match {
      case null => false
      case o: JSONPickle => JsonMethods.jvalueEquals(parsedValue, o.parsedValue)
      case _ => false
    }
    override def hashCode = parsedValue.hashCode
  }
  private[json] class RawStringPickle(override val value: String) extends JSONPickle {
    def parsedValue: JValue =
      jawn.support.json4s.Parser.parseFromString(value) match {
        case Success(json: JValue) => json
        case Failure(e) => throw new PicklingException(s"""failed to parse "${value}" as JSON: ${e.getMessage}""")
      }
  }
  private[json] class JValuePickle(override val parsedValue: JValue) extends JSONPickle {
    // This HAS to be val based on the pickling API.  However, we may never call it for a given pickle,
    // so we'd like to not pay the string rendering tax unless we must.
    override lazy val value: String = JsonMethods.compact(parsedValue)
  }
  object JSONPickle {
    def apply(in: String): JSONPickle = new RawStringPickle(in)
    def fromJValue(in: JValue): JSONPickle = new JValuePickle(in)
  }

  class JSONPickleFormat extends PickleFormat {
    type PickleType = JSONPickle
    type OutputType = Output[String]
    def createBuilder() = new VerifyingJSONPickleBuilder(this, new StringOutput)
    def createBuilder(out: Output[String]): PBuilder = new VerifyingJSONPickleBuilder(this, out)
    def createReader(pickle: JSONPickle) =
      new VerifyingJSONPickleReader(this, IniitalReaderState(pickle.parsedValue))
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
  case class WriteOptionState(val previous: BuilderState) extends BuilderState
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
    // Hackery so we elide option collection types.
    private def isOption(tag: FastTypeTag[_]): Boolean =
      (tag.key startsWith "scala.Option")

    // Here we get notified of object/value-like things.
    override def beginEntry(picklee: Any): PBuilder = withHints { hints =>
      // Here we check to see if we need to serialize a reference.  These are used to avoid circular object
      // dependencies for picklers which have circluarly-references objects.
      if (hints.oid != -1) {
        buf.put("{\"" + REF_ID_FIELD + "\":" + hints.oid + "}")
        state = RefEntryState(state)
      } else if (isOption(hints.tag)) {
        // We expect to be writing a collection, we just ignore the collection aspect.
        state = WriteOptionState(RawEntryState(state, picklee, hints, true))
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
            import JsonMethods._
            buf.put(compact(render(picklee.asInstanceOf[JValue])))
          } else {
            // Note: It's possible the object is empty, so we just put an empty object here,
            // as the type we're serializing may not have any contents.
            // we also serialize the "$type" here if needed.
            buf.put("{")
            if (!hints.isStaticallyElidedType) appendTagString(picklee, hints)
            buf.put("}")
          }
          state = prev
        case MapEntryState(prev, picklee, hints) =>
          // Add the type tag if we don't know it statically.
          if (!hints.isStaticallyElidedType) {
            buf.put(",")
            appendTagString(picklee, hints)
          }
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
        case x: WriteOptionState =>
          // We need to serialize None
          if (length == 0) buf.put("null")
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
        case s: WriteOptionState =>
          // Cheater methods to serialize options as raw values.
          pickler(this)
          this
        case _ => sys.error("Cannot put an element without first specifying a collection.")
      }
    override def endCollection(): Unit =
      state match {
        case s: CollectionState =>
          buf.put("]")
          state = s.previous
        case s: WriteOptionState =>
          state = s.previous
        case _ => sys.error("cannot end a collection when not in collection state!")
      }

    override def result(): JSONPickle = {
      // TODO - verify everything is done, and we have no state stack...
      if (state != EmptyState) sys.error("Failed to close/end all entries and collections!")
      JSONPickle(buf.toString)
    }
  }

  sealed trait ReaderState {
    def previous: ReaderState
    def current: JValue
  }
  // The state where we're looking at a value, but the reader hasn't told us to do anything yet.
  case class RawJsValue(current: JValue, previous: ReaderState) extends ReaderState
  // The state in which we've attempted to read a type tag.
  //  i.e. this means beginEntry has been called.
  case class JsValueWithTag(current: JValue, tagKey: String, previous: ReaderState) extends ReaderState
  // The initial state where we pass parsed JSON and begin parsing.
  case class IniitalReaderState(current: JValue) extends ReaderState {
    def previous: ReaderState = this
  }
  // The state where we are reading elements from a collection.
  case class CollectionReadingState(current: JValue, idx: Int, previous: ReaderState) extends ReaderState

  class VerifyingJSONPickleReader(format: JSONPickleFormat, var state: ReaderState) extends PReader with PickleTools {
    import JSONPickleFormat._

    // Debugging hints
    override def hintTag(tag: FastTypeTag[_]): this.type = {
      //System.err.println(s"hintTag($tag)")
      super.hintTag(tag)
    }
    override def hintStaticallyElidedType(): this.type = {
      //System.err.println(s"hintStaticallyElidedType()")
      super.hintStaticallyElidedType()
    }
    override def pinHints(): this.type = {
      //System.err.println(s"pinHints()")
      super.pinHints()
    }
    override def unpinHints(): this.type = {
      //System.err.println(s"unpinHints()")
      super.pinHints()
    }

    override def beginEntry(): String = withHints { hints =>
      // This should be the default for static picklers.  We don't need runtime reflection,
      // so we just grab tag strings and use that to match known/sealed class hierarchies.
      val tag = currentTag(state.current, hints)
      state = JsValueWithTag(state.current, tag, state.previous)
      tag
    }
    override def endEntry(): Unit = {
      //System.err.println(s"endEntry()")
      // TODO - validate state is correct before we pop the stack.
      state = state.previous
    }

    // Check for primitive at current state.
    override def atPrimitive: Boolean = state match {
      case JsValueWithTag(_, tag, _) =>
        primitives.contains(tag)
      case _ => false
    }

    // Check if the user is aksing for a raw "JValue" so we don't deserialize it.
    private def atJValue: Boolean =
      state match {
        case JsValueWithTag(_, tag, _) => (tag startsWith "org.json4s.JsonAST.")
        case _ => false
      }

    override def readPrimitive(): Any = {
      //System.err.println(s"readPrimitive()")
      def unpickleHelper(value: JValue, tag: String): Any = {
        if (tag startsWith "org.json4s.JsonAST.") value
        else if (primitives.contains(tag)) primitives(tag)(value)
        // NOTE - This is a dirty, rotten hack when the tag.key does not lineup with the data.
        //        We need to figure out hwat's wrong with our SPickles that would case this.
        else value match {
          case x: JString => x.values
          //case x: JDouble => x.values
          case x: JBool => x.value
          // TODO - We need to understand why the tag doesn't say JsonAST here...
          case x: JObject => x
          case _ =>
            // TODO - check to see if we need the old primitiveSeqKeys handling
            // to read a primtiive out of a JArray
            val e = new PicklingException(s"Not a primitive: $tag, found $value")
            e.printStackTrace()
            throw e
        }
      }
      state match {
        case JsValueWithTag(value, tag, _) => unpickleHelper(value, tag)
        // Here we've gotten to a readPrimtive without a beginEntry which reads the tag.  We'll
        //   assume the statically hinted type is the right one
        case _: IniitalReaderState | _: RawJsValue =>
          withHints { hints =>
            unpickleHelper(state.current, hints.tag.key)
          }
        // TODO - Do we need a state where we can read a value if we're in a collection reading state?
        case state =>
          throw new PicklingException(s"Cannot deserialize primitive in state: $state")
      }
    }

    // Check for object at current state, and read fields.
    override def atObject: Boolean =
      // TODO - Check for legit state
      state.current.isInstanceOf[JObject]
    override def readField(name: String): PReader = {
      //System.err.println(s"readField($name)")
      // TODO - assert(atObject) && we're in legit state to read fields...
      val nextState = if (name == DYNAMIC_KEY_FIELD) {
        // TODO - Should we sort here?
        val keys =
          state.current.asInstanceOf[JObject].values.keys.toList.sorted.map(k => JString(k))
        RawJsValue(JArray(keys), state)
      } else RawJsValue(state.current.asInstanceOf[JObject] \ name, state)
      val nested = new VerifyingJSONPickleReader(format, nextState)
      if (this.areHintsPinned) {
        nested.pinHints()
        nested.hints = hints
        // TODO - maybe we modify the state to remember the type tag...
      } else {
        nested.hints = hints
      }
      nested
    }

    // Methods around reading collections.
    override def beginCollection(): PReader = {
      //System.err.println(s"beginCollection()")
      // For now we just migrate into collection reading state.
      state = CollectionReadingState(state.current, 0, state)
      this
    }
    override def readLength(): Int = state match {
      case CollectionReadingState(value, 0, _) =>
        //System.err.println(s"readLength()")
        value match {
          case JNothing => 0
          case JNull => 0 // Hackery for Option handling
          case x: JArray => x.arr.size
          case x => 1 // Hackery for Option handling
        }
      case x => throw new PicklingException(s"Cannot read length when not in collection reading state.")
    }
    override def readElement(): PReader = state match {
      case cs @ CollectionReadingState(value, idx, _) =>
        //System.err.println(s"readElement()")
        // First advance internal state.
        state = cs.copy(idx = idx + 1)
        val subState = value match {
          case x: JArray =>
            RawJsValue(x.apply(idx), state)
          case _ if idx == 0 =>
            RawJsValue(value, state)
        }
        val tmp = new VerifyingJSONPickleReader(format, subState)
        tmp.hints = this.hints // TODO - is this correct?
        tmp
      case x => throw new PicklingException(s"Cannot read an element when not in collection reading state.")
    }
    override def endCollection(): Unit = state match {
      case CollectionReadingState(value, idx, prev) =>
        //System.err.println(s"endCollection()")
        // TODO - Warn if we haven't read all value, maybe
        state = prev
      case _ => throw new PicklingException(s"Cannot end reading a collection when we never started, state: $state")
    }

    // IMPLEMENTATION DETAILS
    // NOTE - most of this can be moved into static helper method
    //        especially the state so we don't create it over and over on every pickle call.

    private val primitives = Map[String, JValue => Any](
      FastTypeTag.Unit.key -> (datum => ()),
      FastTypeTag.Null.key -> (datum => null),
      FastTypeTag.Ref.key -> (datum => lookupUnpicklee(datum match {
        case obj: JObject =>
          (obj \ REF_ID_FIELD) match {
            case JDouble(num) => num.toInt
            case x => unexpectedValue(x, FastTypeTag.Ref)
          }
        case x => unexpectedValue(x, FastTypeTag.Ref)
      })),
      FastTypeTag.Int.key -> (datum => datum match {
        case JDouble(num) => num.toInt
        case x => unexpectedValue(x, FastTypeTag.Int)
      }),
      FastTypeTag.Short.key -> (datum => datum match {
        case JDouble(num) => num.toShort
        case x => unexpectedValue(x, FastTypeTag.Short)
      }),
      FastTypeTag.Double.key -> (datum => datum match {
        case JDouble(num) => num
        case x => unexpectedValue(x, FastTypeTag.Double)
      }),
      FastTypeTag.Float.key -> (datum => datum match {
        case JDouble(num) => num.toFloat
        case x => unexpectedValue(x, FastTypeTag.Float)
      }),
      FastTypeTag.Long.key -> (datum => datum match {
        case JDouble(num) => num.toLong
        case JString(s) => s.toLong
        case x => unexpectedValue(x, FastTypeTag.Long)
      }),
      FastTypeTag.Byte.key -> (datum => datum match {
        case JDouble(num) => num.toByte
        case x => unexpectedValue(x, FastTypeTag.Byte)
      }),
      FastTypeTag.Boolean.key -> (datum => datum match {
        case JBool(b) => b
        case x => unexpectedValue(x, FastTypeTag.Boolean)
      }),
      FastTypeTag.Char.key -> (datum => datum match {
        case JString(s) => s.head
        case x => unexpectedValue(x, FastTypeTag.Char)
      }),
      FastTypeTag.String.key -> (datum => datum match {
        // TODO - where is this coming from... appears to be `Option[String]`, when option is `None`
        // More importantly, why is Jawn returning null instead of JNull?
        case null => null
        case JString(s) => s
        case x => unexpectedValue(x, FastTypeTag.String)
      }),
      FastTypeTag.ArrayByte.key -> (datum => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toByte
            case x => unexpectedValue(x, FastTypeTag.Byte)
          }
        case x => unexpectedValue(x, FastTypeTag.ArrayByte)
      }).toArray),
      FastTypeTag.ArrayShort.key -> (datum => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toShort
            case x => unexpectedValue(x, FastTypeTag.Short)
          }
        case x => unexpectedValue(x, FastTypeTag.ArrayShort)
      }).toArray),
      FastTypeTag.ArrayChar.key -> (datum => (datum match {
        case JArray(arr) =>
          arr map {
            case JString(s) => s.head
            case x: JValue => unexpectedValue(x, FastTypeTag.Char)
          }
        case x => unexpectedValue(x, FastTypeTag.ArrayChar)
      }).toArray),
      FastTypeTag.ArrayInt.key -> { datum =>
        (datum match {
          case JArray(arr) =>
            arr map {
              case JDouble(num) => num.toInt
              case x => unexpectedValue(x, FastTypeTag.Int)
            }
          case x => unexpectedValue(x, FastTypeTag.ArrayInt)
        }).toArray
      },
      FastTypeTag.ArrayLong.key -> (datum => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toLong
            case JString(s) => s.toLong
            case x => unexpectedValue(x, FastTypeTag.Long)
          }
        case x => unexpectedValue(x, FastTypeTag.ArrayLong)
      }).toArray),
      FastTypeTag.ArrayBoolean.key -> (datum => (datum match {
        case JArray(arr) =>
          arr map {
            case JBool(b) => b
            case x => unexpectedValue(x, FastTypeTag.Boolean)
          }
        case x => unexpectedValue(x, FastTypeTag.ArrayBoolean)
      }).toArray),
      FastTypeTag.ArrayFloat.key -> (datum => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num.toFloat
            case x => unexpectedValue(x, FastTypeTag.Float)
          }
        case x => unexpectedValue(x, FastTypeTag.ArrayFloat)
      }).toArray),
      FastTypeTag.ArrayDouble.key -> (datum => (datum match {
        case JArray(arr) =>
          arr map {
            case JDouble(num) => num
            case x => unexpectedValue(x, FastTypeTag.Double)
          }
        case x => unexpectedValue(x, FastTypeTag.ArrayDouble)
      }).toArray))

    private def unexpectedValue(value: JValue, tag: FastTypeTag[_]): Nothing =
      throw new PicklingException("unexpected value: " + value.toString + ", looking for: " + tag)

    /**
     * Reads the pickled "$type" fields from a JObject.
     * Throws an exception if the "$type" fired doesn't exist.
     *
     * Note: This will use some runtime reflection to check if the pickled type still exists.  If it does not,
     * this will use the type hint provided if we're deserializing a known subclass (not an abstract/trait)
     */
    private def readTypeTagKey(obj: JObject, hints: Hints): String = {
      (obj \ TYPE_TAG_FIELD) match {
        case JString(s) => s
        case found => hints.tag.key
      }
    }
    /** Helper to read (or return elided) type tag for the given entry. */
    private def currentTag(current: JValue, hints: Hints): String = {
      current match {
        case JNull => FastTypeTag.Null.key
        case JNothing => FastTypeTag.Nothing.key
        case obj: JObject =>
          (obj \ REF_ID_FIELD) match {
            case JDouble(num) => FastTypeTag.Ref.key
            // Not a reference type.
            case _ =>
              if (hints.isElidedType || hints.isStaticallyElidedType || hints.isDynamicallyElidedType) hints.tag.key
              else readTypeTagKey(obj, hints)
          }
        case _ if (hints.tag != null) => hints.tag.key
        case _ =>
          // TODO - This should be an error.  We need  a tag and we have NO IDEA what we are.
          throw new PicklingException(s"Attempting to find tag in $current, but hints has ${hints.tag}")
      }
    }
  }
}
