package sbt.pickling.spec

import org.specs2._
import scala.pickling._, sbt.pickling.json._
import matcher.MatchResult
import java.io.File
import java.net.URI
import sbt.protocol
import sbt.protocol.Message

class ProtocolSpec extends Specification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check custom JSON pickling.

  basic types should
    round trip.                                                 $basicTypes
  array types should
    round trip.                                                 $arrayTypes
  messages should
    round trip.                                                 $messages

                                                                """
  def basicTypes = {
    // simple data type
    roundTrip("Foo")
    roundTrip(new File("/tmp"))
    roundTrip(new URI("/tmp"))
    roundTrip(true)
    roundTrip(false)
    roundTrip(10: Short)
    roundTrip(11)
    roundTrip(12L)
    roundTrip(13.0f)
    roundTrip(14.0)
    roundTrip(None: Option[String])         // roundTrip(None) must fail to compile
    roundTrip(Some("Foo"): Option[String])  // roundTrip(Some("Foo")) must fail to compile
    roundTrip(Some(true): Option[Boolean])  // roundTrip(Some(true)) must fail to compile
    roundTrip(Some(10): Option[Int])        // roundTrip(Some(10)) must fail to compile
  }

  def arrayTypes = {
    // arrays
    roundTrip(Nil: List[String])
    roundTrip(Array(): Array[String])
    roundTrip(Vector(): Vector[String])
    roundTrip(Seq("Bar", "Baz").toArray)
    roundTrip(Seq("Bar", "Baz").toVector)
    roundTrip(Seq("Bar", "Baz").toList)
    roundTrip(Seq(1, 2, 3).toVector)
    roundTrip(Seq(true, false, true, true, false).toVector)
  }

  val key = protocol.AttributeKey("name", protocol.TypeInfo("java.lang.String"))
  val build = new java.net.URI("file:///test/project")
  val projectRef = protocol.ProjectReference(build, "test")
  val scope = protocol.SbtScope(project = Some(projectRef))
  val scopedKey = protocol.ScopedKey(key, scope)

  def messages = {
    // messages
    roundTripMessage(protocol.KillServerRequest())
    roundTripMessage(protocol.ReadLineRequest(42, "HI", true))
    roundTripMessage(protocol.ReadLineResponse(Some("line")))
    roundTripMessage(protocol.ConfirmRequest(43, "msg"))
    roundTripMessage(protocol.ReadLineResponse(Some("line")))
    roundTripMessage(protocol.ReceivedResponse())
    roundTripMessage(protocol.CommandCompletionsRequest("He", 2))
    // roundTripMessage(protocol.CommandCompletionsResponse(Set(protocol.Completion("llo", "Hello", true))))
    roundTripMessage(protocol.ListenToEvents())
    roundTripMessage(protocol.ListenToBuildChange())
    roundTripMessage(protocol.ExecutionRequest("test command string"))
    // roundTripMessage(protocol.ListenToValue(scopedKey))
    roundTripMessage(protocol.CancelExecutionRequest(1))
    roundTripMessage(protocol.ErrorResponse("ZOMG"))
    roundTripMessage(protocol.CancelExecutionResponse(false))
  }
  def roundTripMessage[A <: Message: FastTypeTag: SPickler: Unpickler](a: A) = {
    val json: String = a.pickle.value
    System.err.println(s"json: $json")
    val parsed = SpecsUtil.parseMessage(json)
    a must_== parsed
  }
  def roundTrip[A: FastTypeTag: SPickler: Unpickler](x: A): MatchResult[Any] =
    roundTripBase[A](x)((a, b) =>
      a must_== b
    ) { (a, b) =>
      a.getMessage must_== b.getMessage
    }
  def roundTripBase[A: FastTypeTag: SPickler: Unpickler](a: A)(f: (A, A) => MatchResult[Any])(e: (Throwable, Throwable) => MatchResult[Any]): MatchResult[Any] = {
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
}
