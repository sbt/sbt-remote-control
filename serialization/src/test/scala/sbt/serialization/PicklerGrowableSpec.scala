package sbt.serialization.spec

import org.junit.Assert._
import org.junit._
import sbt.serialization._
import JUnitUtil._

case class Foo(x: Int, y: Option[Int])
object Foo {
  implicit val pickler = genPickler[Foo]
  implicit val unpickler = genUnpickler[Foo]
}

class PicklerGrowableTest {
  @Test
  def testUnpickleWithExtra: Unit = {
    SerializedValue.fromJsonString(extraFieldExample).parse[Foo].get must_== Foo(1, Some(1))
  }

  @Test
  def testUnpickleWithMissing: Unit = {
    SerializedValue.fromJsonString(missingFieldExample).parse[Foo].get must_== Foo(1, None)
  }

  lazy val extraFieldExample = """{
    |  "$type": "sbt.serialization.spec.Foo",
    |  "x": 1,
    |  "y": 1,
    |  "z": 1
    |}""".stripMargin
  lazy val missingFieldExample = """{
    |  "$type": "sbt.serialization.spec.Foo",
    |  "x": 1
    |}""".stripMargin
}
