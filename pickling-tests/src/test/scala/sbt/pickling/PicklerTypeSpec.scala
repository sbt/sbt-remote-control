package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import scala.pickling._, sbt.pickling.json._
import SpecsUtil._
import JUnitUtil._

class PicklerTypeTest {
  @Test
  def testPickleApple: Unit = {
    Apple(1).pickle.value must_== appleExample
  }

  @Test
  def testUnpickleApple: Unit = {
    appleExample.unpickle[Apple] must_== Apple(1)
  }

  @Test
  def testUnpickleFruit: Unit = {
    appleExample.unpickle[Fruit] must_== Apple(1)
  }

  @Test
  def testUnpickleAny: Unit = {
    appleExample.unpickle[Any] must_== Apple(1)
  }

  @Test
  def testUnpickleOrange: Unit = {
    appleExample.unpickle[Orange] must_== Orange(1)
  }

  @Test
  def testUnpickleOrangeFromUnknown: Unit = {
    unknownTypeExample.unpickle[Orange] must_== Orange(1)
  }

  @Test
  def testUnpickleFruitFromUnknown: Unit = {
    try {
      unknownTypeExample.unpickle[Fruit]
      sys.error("didn't fail")
    } catch {
      case _: PicklingException => ()
    }
  }

  lazy val appleExample = """{
    |  "$type": "sbt.pickling.spec.Apple",
    |  "x": 1
    |}""".stripMargin
  lazy val unknownTypeExample = """{
    |  "$type": "something_unknown",
    |  "x": 1
    |}""".stripMargin
}

sealed trait Fruit
case class Apple(x: Int) extends Fruit
case class Orange(x: Int) extends Fruit
