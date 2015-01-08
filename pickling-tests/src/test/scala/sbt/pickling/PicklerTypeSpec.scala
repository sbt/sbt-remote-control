package sbt.pickling.spec

import org.junit.Assert._
import org.junit._
import scala.pickling.{ PickleOps, UnpickleOps, PicklingException }
import sbt.pickling._, sbt.pickling.json._
import sbt.serialization._
import SpecsUtil._
import JUnitUtil._

sealed trait Fruit
case class Apple(x: Int) extends Fruit
object Apple {
  implicit val pickler = genPickler[Apple]
  implicit val unpickler = genUnpickler[Apple]
}
case class Orange(x: Int) extends Fruit
object Orange {
  implicit val pickler = genPickler[Orange]
  implicit val unpickler = genUnpickler[Orange]
}

object Fruit {
  implicit val pickler = genPickler[Fruit]
  implicit val unpickler = genUnpickler[Fruit]
}

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
