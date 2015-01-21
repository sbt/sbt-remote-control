package sbt.serialization.spec

import org.junit.Assert._
import org.junit._
import scala.pickling.{ PickleOps, UnpickleOps, PicklingException }
import sbt.serialization._, sbt.serialization.functions._, sbt.serialization.json._
import JUnitUtil._
import scala.pickling.ops._

object Fruits {
  val coreProtocol: CustomPicklerUnpickler = new CustomPicklerUnpickler {}
  import coreProtocol._

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
}

class PicklerTypeTest {
  import Fruits._

  @Test
  def testPickleApple: Unit = {
    assertEquals("Apple(1)", appleExample, Apple(1).pickle.value)
  }

  @Test
  def testUnpickleApple: Unit = {
    appleExample.unpickle[Apple] must_== Apple(1)
  }

  @Test
  def testUnpickleFruit: Unit = {
    appleExample.unpickle[Fruit] must_== Apple(1)
  }

  // TODO - We need to fix this in the genUnpickler macro itself
  @Test @Ignore
  def testUnpickleOrange: Unit = {
    appleExample.unpickle[Orange] must_== Orange(1)
  }

  // TODO - We need to fix this in the genUnpickler macro itself
  @Test @Ignore
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

  lazy val appleExample = """{"x":1,"$type":"sbt.serialization.spec.Fruits.Apple"}""".stripMargin
  lazy val unknownTypeExample = """{
    |  "$type": "something_unknown",
    |  "x": 1
    |}""".stripMargin
}
