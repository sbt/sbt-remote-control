package sbt.pickling.spec

import collection.immutable.::
import org.specs2._
import scala.pickling._, sbt.pickling.json._

class PicklerTypeSpec extends Specification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check custom JSON pickling.

  { "$$type": "x.y.Apple", "x": 1 } should
    be pickled from Apple(1)                                    $pickleApple
    unpickle[Apple] to Apple(1)                                 $unpickleApple
    unpickle[Fruit] to Apple(1)                                 $unpickleFruit
    unpickle[Any] to Apple(1)                                   $unpickleAny
    unpickle[Orange] to Orange(1).                              $unpickleOrange

  { "$$type": "unknown", "x": 1 } should
    unpickle[Orange] to Orange(1)                               $unpickleOrangeFromUnknown
    unpickle[Fruit] should error.                               $unpickleFruitFromUnknown
                                                                """
  
  val appleExample = """{
    |  "$type": "sbt.pickling.spec.Apple",
    |  "x": 1
    |}""".stripMargin
  val unknownTypeExample = """{
    |  "$type": "something_unknown",
    |  "x": 1
    |}""".stripMargin
  def pickleApple = Apple(1).pickle.value must_== appleExample
  def unpickleApple = appleExample.unpickle[Apple] must_== Apple(1)
  def unpickleFruit = appleExample.unpickle[Fruit] must_== Apple(1)
  def unpickleAny = appleExample.unpickle[Any] must_== Apple(1)
  def unpickleOrange = appleExample.unpickle[Orange] must_== Orange(1)
  def unpickleOrangeFromUnknown = unknownTypeExample.unpickle[Orange] must_== Orange(1)
  def unpickleFruitFromUnknown =
    try {
      unknownTypeExample.unpickle[Fruit]
      failure
    } catch {
      case _: PicklingException => success
    }
}

sealed trait Fruit
case class Apple(x: Int) extends Fruit
case class Orange(x: Int) extends Fruit
