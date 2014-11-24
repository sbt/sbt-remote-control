package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import scala.pickling._, sbt.pickling.json._
import SpecsUtil._
import JUnitUtil._

class PicklerGrowableTest {
  @Test
  def testUnpickleWithExtra: Unit = {
    extraFieldExample.unpickle[Foo] must_== Foo(1, Some(1))
  }

  @Test
  def testUnpickleWithMissing: Unit = {
    missingFieldExample.unpickle[Foo] must_== Foo(1, None)
  }

  lazy val extraFieldExample = """{
    |  "$type": "sbt.pickling.spec.Foo",
    |  "x": 1,
    |  "y": 1,
    |  "z": 1
    |}""".stripMargin
  lazy val missingFieldExample = """{
    |  "$type": "sbt.pickling.spec.Foo",
    |  "x": 1
    |}""".stripMargin
}

case class Foo(x: Int, y: Option[Int])
