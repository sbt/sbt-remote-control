package sbt.pickling.spec

import org.specs2._
import scala.pickling._, sbt.pickling.json._
import matcher.MatchResult
import java.io.File

class ProtocolSpec extends Specification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check custom JSON pickling.

  basic types should
    round trip.                                                 $basicTypes
  array types should
    round trip.                                                 $arrayTypes

                                                                """
  def basicTypes =
    {
      // simple data type
      roundTrip("Foo")
      roundTrip(new File("/tmp"))
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

  def arrayTypes =
    {
      // arrays
      roundTrip(Nil: List[String])
      roundTrip(Array(): Array[String])
      roundTrip(Seq("Bar", "Baz").toArray)
      // roundTrip(Seq(1, 2, 3).toVector)
      // roundTrip(Seq(true, false, true, true, false).toVector)
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
    System.err.println(s"A: $tag")
    val parsed = json.unpickle[A]
    (a, parsed) match {
      case (a: Throwable, parsed: Throwable) => e(a, parsed)
      case _ => f(a, parsed)
    }
  }
}
