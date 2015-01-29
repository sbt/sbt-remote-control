package sbt.serialization.spec

import org.junit.Assert._
import org.junit._
import scala.pickling.{ PicklingException }
import sbt.serialization._
import JUnitUtil._

object Fruits {
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
    assertEquals("Apple(1)", appleExample, SerializedValue(Apple(1)).toJsonString)
  }

  @Test
  def testUnpickleApple: Unit = {
    SerializedValue.fromJsonString(appleExample).parse[Apple].get must_== Apple(1)
  }

  @Test
  def testUnpickleFruit: Unit = {
    SerializedValue.fromJsonString(appleExample).parse[Fruit].get must_== Apple(1)
  }

  @Test
  def testUnpickleOrange: Unit = {
    SerializedValue.fromJsonString(appleExample).parse[Orange].get must_== Orange(1)
  }

  @Test
  def testUnpickleOrangeFromUnknown: Unit = {
    SerializedValue.fromJsonString(unknownTypeExample).parse[Orange].get must_== Orange(1)
  }

  @Test
  def testUnpickleFruitFromUnknown: Unit = {
    try {
      SerializedValue(unknownTypeExample).parse[Fruit].get
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
