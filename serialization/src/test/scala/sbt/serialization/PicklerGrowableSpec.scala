package sbt.serialization.spec

import org.junit.Assert._
import org.junit._
import scala.pickling.{ PickleOps, UnpickleOps }
import scala.pickling.ops._
import sbt.serialization._, sbt.serialization.functions._, sbt.serialization.json._
import JUnitUtil._

case class Foo(x: Int, y: Option[Int])
object Foo {
  val coreProtocol: CustomPicklerUnpickler = new CustomPicklerUnpickler {}
  import coreProtocol._

  implicit val pickler = genPickler[Foo]
  implicit val unpickler = genUnpickler[Foo]
}

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
