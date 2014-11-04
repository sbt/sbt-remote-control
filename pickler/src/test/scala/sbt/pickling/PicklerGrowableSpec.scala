package sbt.pickling.spec

import org.specs2._
import scala.pickling._, sbt.pickling.json._

class PicklerGrowableSpec extends Specification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check custom JSON pickling.

  { "$$type": "x.y.Foo", "x": 1, "y": 1, "z": 1 } should
    unpickle[Foo] to Foo(1, Some(1)) by ignoring z.             $unpickleWithExtra

  { "$$type": "x.y.Foo", "x": 1 } should
    unpickle[Foo] to Foo(1, None) by inferring y.               $unpickleWithMissing
                                                                """
  
  val extraFieldExample = """{
    |  "$type": "sbt.pickling.spec.Foo",
    |  "x": 1,
    |  "y": 1,
    |  "z": 1
    |}""".stripMargin
  val missingFieldExample = """{
    |  "$type": "sbt.pickling.spec.Foo",
    |  "x": 1
    |}""".stripMargin
  def unpickleWithExtra = extraFieldExample.unpickle[Foo] must_== Foo(1, Some(1))
  def unpickleWithMissing = missingFieldExample.unpickle[Foo] must_== Foo(1, None)
}

case class Foo(x: Int, y: Option[Int])
