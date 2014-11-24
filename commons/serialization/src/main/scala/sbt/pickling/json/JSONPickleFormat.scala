package sbt.pickling

import scala.pickling._
import scala.pickling.internal.lookupUnpicklee
import scala.reflect.runtime.universe._
import definitions._
import org.json4s._
import scala.util.parsing.json.JSONFormat.quoteString
import scala.collection.mutable.{ StringBuilder, Stack }
import scala.util.{ Success, Failure }

package json {
  object `package` extends CustomPicklerUnpickler {
    implicit val pickleFormat: JSONPickleFormat = new JSONPickleFormat
    implicit def toJSONPickle(value: String): JSONPickle = JSONPickle(value)
    implicit def toUnpickleOps(value: String): UnpickleOps = new UnpickleOps(JSONPickle(value))
  }

  case class JSONPickle(value: String) extends Pickle {
    type ValueType = String
    type PickleFormatType = JSONPickleFormat
  }

  class JSONPickleFormat extends PickleFormat {
    type PickleType = JSONPickle
    type OutputType = Output[String]
    def createBuilder() = new JSONPickleBuilder(this, new StringOutput)
    def createBuilder(out: Output[String]): PBuilder = new JSONPickleBuilder(this, out)
    def createReader(pickle: JSONPickle, mirror: Mirror) = {
      jawn.support.json4s.Parser.parseFromString(pickle.value) match {
        case Success(json) => new JSONPickleReader(json, mirror, this)
        case Failure(e) => throw new PicklingException("failed to parse \"" + pickle.value + "\" as JSON: " + e.getMessage)
      }
    }
  }

  class JSONPickleBuilder(format: JSONPickleFormat, buf: Output[String]) extends PBuilder with PickleTools {
    private var nindent = 0
    private def indent() = nindent += 1
    private def unindent() = nindent -= 1
    private var pendingIndent = false
    private var lastIsBrace = false
    private var lastIsBracket = false
    private def append(s: String) = {
      val sindent = if (pendingIndent) "  " * nindent else ""
      buf.put(sindent + s)
      pendingIndent = false
      val trimmed = s.trim
      if (trimmed.nonEmpty) {
        val lastChar = trimmed.last
        lastIsBrace = lastChar == '{'
        lastIsBracket = lastChar == '['
      }
    }
    private def appendLine(s: String = "") = {
      append(s + "\n")
      pendingIndent = true
    }
    private val tags = new Stack[FastTypeTag[_]]()
    private def pickleArray(arr: Array[_], tag: FastTypeTag[_]) = {
      unindent()
      appendLine("[")
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
      appendLine("")
      append("]")
      indent()
    }
    private def pickleSeq(seq: Seq[_], tag: FastTypeTag[_]) = {
      unindent()
      appendLine("[")
      pushHints()
      hintStaticallyElidedType()
      hintTag(tag)
      pinHints()
      seq foreach { x =>
        putElement(b => b.beginEntry(x).endEntry())
      }
      popHints()
      appendLine("")
      append("]")
      indent()
    }
    private val primitives = Map[String, Any => Unit](
      FastTypeTag.Unit.key -> ((picklee: Any) => append("\"()\"")),
      FastTypeTag.Null.key -> ((picklee: Any) => append("null")),
      FastTypeTag.Ref.key -> ((picklee: Any) => throw new Error("fatal error: shouldn't be invoked explicitly")),
      FastTypeTag.Int.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Long.key -> ((picklee: Any) => append(picklee.toString)), // append("\"" + quoteString(picklee.toString) + "\"")),
      FastTypeTag.Short.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Double.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Float.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Boolean.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Byte.key -> ((picklee: Any) => append(picklee.toString)),
      FastTypeTag.Char.key -> ((picklee: Any) => append("\"" + quoteString(picklee.toString) + "\"")),
      FastTypeTag.String.key -> ((picklee: Any) => append("\"" + quoteString(picklee.toString) + "\"")),
      FastTypeTag.ArrayByte.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Byte]], FastTypeTag.Byte)),
      FastTypeTag.ArrayShort.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Short]], FastTypeTag.Short)),
      FastTypeTag.ArrayChar.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Char]], FastTypeTag.Char)),
      FastTypeTag.ArrayInt.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Int]], FastTypeTag.Int)),
      FastTypeTag.ArrayLong.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Long]], FastTypeTag.Long)),
      FastTypeTag.ArrayBoolean.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Boolean]], FastTypeTag.Boolean)),
      FastTypeTag.ArrayFloat.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Float]], FastTypeTag.Float)),
      FastTypeTag.ArrayDouble.key -> ((picklee: Any) => pickleArray(picklee.asInstanceOf[Array[Double]], FastTypeTag.Double)))

    private def isIterable(tag: FastTypeTag[_]): Boolean =
      ManifestUtil.isApproxIterable(tag)
    private def isOption(tag: FastTypeTag[_]): Boolean =
      (tag.key startsWith "scala.Option[")
    private def isJValue(tag: FastTypeTag[_]): Boolean =
      (tag.key startsWith "org.json4s.JsonAST.")

    def beginEntry(picklee: Any): PBuilder = withHints { hints =>
      indent()
      if (hints.oid != -1) {
        tags.push(FastTypeTag.Ref)
        append("{ \"$ref\": " + hints.oid + " }")
      } else {
        tags.push(hints.tag)
        if (primitives.contains(hints.tag.key)) {
          primitives(hints.tag.key)(picklee)
          // if (hints.isElidedType) primitives(hints.tag.key)(picklee)
          // else {
          //   appendLine("{")
          //   appendLine("\"$type\": \"" + hints.tag.key + "\",")
          //   append("\"value\": ")
          //   indent()
          //   primitives(hints.tag.key)(picklee)
          //   unindent()
          //   appendLine("")
          //   unindent()
          //   append("}")
          //   indent()
          // }
        } else if (isJValue(hints.tag)) {
          appendJson(picklee match {
            case json: JValue => json
            case _ => throw new PicklingException("json expected but found: " + picklee.toString)
          })
        } else if (isIterable(hints.tag)) ()
        else if (isOption(hints.tag)) ()
        else {
          appendLine("{")
          // Ignore isStaticallyElidedType. Always output $type.
          val ts =
            if (hints.tag.key.contains("anonfun$")) picklee.getClass.getName
            else hints.tag.key
          append("\"$type\": \"" + ts + "\"")
        }
      }
      this
    }
    def appendJson(json: JValue): Unit = {
      import org.json4s.native.JsonMethods._
      append(compact(render(json)))
    }
    def putField(name: String, pickler: PBuilder => Unit): PBuilder = {
      // assert(!primitives.contains(tags.top.key), tags.top)
      if (!lastIsBrace) appendLine(",") // TODO: very inefficient, but here we don't care much about performance
      append("\"" + name + "\": ")
      pickler(this)
      this
    }
    def endEntry(): Unit = {
      unindent()
      val tag = tags.pop()
      if (primitives.contains(tag.key)) () // do nothing
      else if (isJValue(tag)) ()
      else if (isIterable(tag)) ()
      else if (isOption(tag)) ()
      else { appendLine(); append("}") }
    }
    def beginCollection(length: Int): PBuilder = {
      // putField("$elems", b => ())
      appendLine("[")
      // indent()
      this
    }
    def putElement(pickler: PBuilder => Unit): PBuilder = {
      if (!lastIsBracket) appendLine(",") // TODO: very inefficient, but here we don't care much about performance
      pickler(this)
      this
    }
    def endCollection(): Unit = {
      appendLine()
      append("]")
      // unindent()
    }
    def result(): JSONPickle = {
      assert(tags.isEmpty, tags)
      JSONPickle(buf.toString)
    }
  }

  class JSONPickleReader(var datum: Any, val mirror: Mirror, format: JSONPickleFormat) extends PReader with PickleTools {
    private var lastReadTag: FastTypeTag[_] = null
    private val primitives = Map[String, () => Any](
      FastTypeTag.Unit.key -> (() => ()),
      FastTypeTag.Null.key -> (() => null),
      FastTypeTag.Ref.key -> (() => lookupUnpicklee(datum match {
        case obj: JObject =>
          (obj \ "$ref") match {
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
              case JField("$ref", _) => true
              case _ => false
            }).isDefined => FastTypeTag.Ref
            case obj: JObject if (obj findField {
              case JField("$type", _) => true
              case _ => false
            }).isDefined =>
              (obj \ "$type") match {
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
          mkNestedReader(obj \ "$value").primitives(lastReadTag.key)()
        case x if atPrimitive =>
          primitives(lastReadTag.key)()
        case JString(s) =>
          s
        case _ =>
          throw new PicklingException(s"""$datum cannot be read as a primitive""")
      }
    }
    def atObject: Boolean = datum.isInstanceOf[JObject]
    def readField(name: String): JSONPickleReader = {
      datum match {
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

  object FakeTag {
    val File: String = "java.io.File"
  }
}
