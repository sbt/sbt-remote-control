package sbt.protocol.spec

import org.junit.Assert._
import org.junit._
import sbt.protocol.Message
import scala.pickling.PickleOps, sbt.serialization._, sbt.serialization.json._
import scala.pickling.static._
import scala.pickling.Defaults.pickleOps

object JUnitUtil {
  private def addWhatWeWerePickling[T, U](t: T)(body: => U): U = try body
  catch {
    case e: Throwable =>
      e.printStackTrace()
      throw new AssertionError(s"Crash round-tripping ${t.getClass.getName}: value was: ${t}", e)
  }

  def roundTripArray[A](x: Array[A])(implicit ev0: SPickler[Array[A]], ev1: Unpickler[Array[A]]): Unit =
    roundTripBase[Array[A]](x)((a, b) =>
      assertEquals(a.toList, b.toList)) { (a, b) =>
      assertEquals(s"Failed to round trip $x via ${implicitly[SPickler[Array[A]]]} and ${implicitly[Unpickler[Array[A]]]}", a.getMessage, b.getMessage)
    }
  def roundTrip[A: SPickler: Unpickler](x: A): Unit =
    roundTripBase[A](x)((a, b) =>
      assertEquals(a, b)) { (a, b) =>
      assertEquals(s"Failed to round trip $x via ${implicitly[SPickler[A]]} and ${implicitly[Unpickler[A]]}", a.getMessage, b.getMessage)
    }
  def roundTripBase[A: SPickler: Unpickler](a: A)(f: (A, A) => Unit)(e: (Throwable, Throwable) => Unit): Unit = addWhatWeWerePickling(a) {
    import sbt.serialization._, sbt.serialization.json._
    val json = SerializedValue(a).toJsonString
    //System.err.println(s"json: $json")
    val parsed = SerializedValue.fromJsonString(json).parse[A].get
    (a, parsed) match {
      case (a: Throwable, parsed: Throwable) => e(a, parsed)
      case _ => f(a, parsed)
    }
  }
  implicit class AnyOp[A](a: A) {
    def must_==(b: A): Unit = assertEquals(b, a)
  }

  // TODO get rid of pickleMessage and parseMessage once pickle/unpickle are not macros
  def pickleMessage(m: Message): String =
    SerializedValue(m).toJsonString

  def parseMessage(s: String): Message =
    SerializedValue.fromJsonString(s).parse[Message].get
}
