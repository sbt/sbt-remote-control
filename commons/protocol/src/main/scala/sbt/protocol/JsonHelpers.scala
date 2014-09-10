package sbt.protocol

import play.api.libs.json._
import Json._
import play.api.libs.functional.syntax._

private[sbt] object JsonHelpers {
  import play.api.libs.json._
  import play.api.libs.json.Json._
  import play.api.libs.functional.syntax._
  import play.api.libs.json.Reads._
  import play.api.libs.json.Writes._
  import play.api.libs.functional.FunctionalBuilder
  import play.api.data.validation.ValidationError

  final val hexArray: Array[Char] = "0123456789abcdef".toCharArray

  final def bytesToHex(bytes: Array[Byte]): String = {
    val hexChars: Array[Char] = new Array[Char](bytes.length * 2)
    var j: Int = 0
    while (j < bytes.length) {
      val v = bytes(j) & 0xFF
      hexChars(j * 2) = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0F)
      j += 1
    }
    new String(hexChars)
  }

  final def hexToBytes(hex: String): Array[Byte] = {
    val len = hex.length
    val s = hex.toUpperCase
    val result: Array[Byte] = new Array[Byte](len / 2)
    var i = 0
    while (i < len) {
      result(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).asInstanceOf[Byte]
      i += 2
    }
    result
  }

  def extractTagged[T](key: String, tag: String)(reads: Reads[T]): Reads[T] =
    (__ \ key).read[String](pattern(tag.r)) ~> reads

  def extractType[T](tag: String)(reads: Reads[T]): Reads[T] =
    extractTagged("type", tag)(reads)

  def emitTagged[T](key: String, tag: String)(bodyFunc: T => JsObject): Writes[T] = new Writes[T] {
    def writes(in: T): JsValue =
      obj(key -> tag) ++ bodyFunc(in)
  }

  def emitType[T](tag: String)(bodyFunc: T => JsObject): Writes[T] =
    emitTagged("type", tag)(bodyFunc)

  def emitTypedValue(tag: String, value: (String, JsValueWrapper)*): JsValue =
    obj("type" -> tag) ++ obj(value: _*)

}
