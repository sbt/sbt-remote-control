package sbt.protocol.spec

import sbt.protocol.Message
import org.junit.Assert._
import org.junit._
import scala.pickling.{ SPickler, Unpickler }
import sbt.serialization._
import scala.pickling.ops._

object JUnitMessageUtil {
  private def addWhatWeWerePickling[T, U](t: T)(body: => U): U = try body
  catch {
    case e: Throwable =>
      e.printStackTrace()
      throw new AssertionError(s"Crash round-tripping ${t.getClass.getName}: value was: ${t}", e)
  }

  def roundTripMessage(message: Message): Unit = addWhatWeWerePickling(message) {
    import scala.pickling._, sbt.serialization.json._
    val expectedPickler = implicitly[SPickler[Message]]
    val expectedUnpickler = implicitly[Unpickler[Message]]
    val json: String = try message.pickle.value
    catch {
      case t: Throwable =>
        System.err.println(s"pickle of ${message.getClass.getName} failed: ${t.getClass.getName} ${t.getMessage}")
        System.err.println(s"value we failed to pickle: $message")
        System.err.println(s"implicit pickler was: ${expectedPickler.getClass.getName}")
        throw t
    }
    val parsed = try {
      SpecsUtil.parseMessage(json)
    } catch {
      case t: Throwable =>
        System.err.println(s"parse of ${message.getClass.getName} failed: ${t.getClass.getName} ${t.getMessage}")
        System.err.println(s"value we had pickled: $message")
        System.err.println(s"json was: $json")
        System.err.println(s"implicit unpickler was: ${expectedUnpickler.getClass.getName}")
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
        System.err.println(s"implicit pickler was: ${expectedPickler.getClass.getName} unpickler: ${expectedUnpickler.getClass.getName}")
        throw t
    }
  }
}
