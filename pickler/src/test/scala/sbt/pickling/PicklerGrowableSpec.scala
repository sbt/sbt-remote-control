package sbt.pickling.spec

import org.specs2._
import scala.pickling._, sbt.pickling.json._

class PicklerGrowableSpec extends Specification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check custom JSON pickling.

  { "$$type": "x.y.Foo", "x": 1, "y": 1, "z": 1 } should
    unpickle[Foo] to Foo(1, Some(1)) by ignoring z.             $unpickleWithExtra
                                                                """
  
  val extraFieldExample = """{
    |  "$type": "sbt.pickling.spec.FooV2",
    |  "x": 1,
    |  "y": 1,
    |  "z": 1
    |}""".stripMargin
  def unpickleWithExtra = extraFieldExample.unpickle[Foo] must_== Foo(1, Some(1))
}

case class Foo(x: Int, y: Option[Int])
