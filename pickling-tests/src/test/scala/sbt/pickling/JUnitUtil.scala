package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import sbt.protocol.Message
import scala.pickling._, sbt.pickling.json._

object JUnitUtil {
  private def addWhatWeWerePickling[T, U](t: T)(body: => U): U = try body
  catch {
    case e: Throwable =>
      throw new AssertionError(s"Crash round-tripping ${t.getClass.getName}: value was: ${t}", e)
  }

  def roundTripMessage(message: Message): Unit = addWhatWeWerePickling(message) {
    import scala.pickling._, sbt.pickling.json._
    val expectedPickler = implicitly[SPickler[Message]]
    val expectedUnpickler = implicitly[Unpickler[Message]]
    val json: String = try message.pickle.value
    catch {
      case t: Throwable =>
        System.err.println(s"pickle of ${message.getClass.getName} failed: ${t.getClass.getName} ${t.getMessage}")
        System.err.println(s"value we failed to pickle: $message")
        System.err.println(s"implicit pickler should have been used: ${expectedPickler.getClass.getName}")
        throw t
    }
    val parsed = try {
      SpecsUtil.parseMessage(json)
    } catch {
      case t: Throwable =>
        System.err.println(s"parse of ${message.getClass.getName} failed: ${t.getClass.getName} ${t.getMessage}")
        System.err.println(s"value we had pickled: $message")
        System.err.println(s"json was: $json")
        System.err.println(s"implicit unpickler should have been used: ${expectedUnpickler.getClass.getName}")
        throw t
    }
    try {
      assertEquals(message, parsed)
    } catch {
      case t: Throwable =>
        System.err.println(s"roundtrip of ${message.getClass.getName} failed: ${t.getClass.getName} ${t.getMessage}")
        System.err.println(s"value we failed to roundtrip: $message")
        System.err.println(s"what we parsed was:           $parsed")
        System.err.println(s"json was: $json")
        System.err.println(s"implicit pickler should have been used: ${expectedPickler.getClass.getName} unpickler: ${expectedUnpickler.getClass.getName}")
        throw t
    }
  }
  def roundTrip[A: FastTypeTag: SPickler: Unpickler](x: A): Unit =
    roundTripBase[A](x)((a, b) =>
      assertEquals(a, b)
    ) { (a, b) =>
      assertEquals(a.getMessage, b.getMessage)
    }
  def roundTripBase[A: FastTypeTag: SPickler: Unpickler](a: A)(f: (A, A) => Unit)(e: (Throwable, Throwable) => Unit): Unit = addWhatWeWerePickling(a) {
    import scala.pickling._, sbt.pickling.json._
    val json = a.pickle.value
    //System.err.println(s"json: $json")
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
