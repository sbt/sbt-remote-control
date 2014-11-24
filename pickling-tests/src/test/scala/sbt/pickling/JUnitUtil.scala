package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import sbt.protocol.Message
import scala.pickling._, sbt.pickling.json._

object JUnitUtil {
  def roundTripMessage[A <: Message: FastTypeTag: SPickler: Unpickler](a: A): Unit = {
    import scala.pickling._, sbt.pickling.json._
    val json: String = a.pickle.value
    System.err.println(s"json: $json")
    val parsed = SpecsUtil.parseMessage(json)
    assertEquals(a, parsed)
  }
  def roundTrip[A: FastTypeTag: SPickler: Unpickler](x: A): Unit =
    roundTripBase[A](x)((a, b) =>
      assertEquals(a, b)
    ) { (a, b) =>
      assertEquals(a.getMessage, b.getMessage)
    }
  def roundTripBase[A: FastTypeTag: SPickler: Unpickler](a: A)(f: (A, A) => Unit)(e: (Throwable, Throwable) => Unit): Unit = {
    import scala.pickling._, sbt.pickling.json._
    val json = a.pickle.value
    System.err.println(s"json: $json")
    val tag = implicitly[FastTypeTag[A]]
    // System.err.println(s"A: $tag")
    val parsed = json.unpickle[A]
    (a, parsed) match {
      case (a: Throwable, parsed: Throwable) => e(a, parsed)
      case _ => f(a, parsed)
    }
  }
  implicit class AnyOp[A](a: A) {
    def must_==(b: A): Unit = assertEquals(b, a)
  }
}
