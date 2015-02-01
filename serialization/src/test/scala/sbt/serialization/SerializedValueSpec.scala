package sbt.serialization.spec

import org.junit.Assert._
import org.junit._
import sbt.serialization._
import JUnitUtil._

private final case class Apple(foo: Int) extends Fruit
private object Apple {
  implicit val pickler = SPickler.generate[Apple]
  implicit val unpickler = Unpickler.generate[Apple]
}
private final case class Orange(bar: String) extends Fruit
private object Orange {
  implicit val pickler = SPickler.generate[Orange]
  implicit val unpickler = Unpickler.generate[Orange]
}
private sealed trait Fruit
private object Fruit {
  implicit val pickler = SPickler.generate[Fruit]
  implicit val unpickler = Unpickler.generate[Fruit]
}

class SerializedValueTest {
  @Test
  def serializedValueParses(): Unit = {
    assertEquals(Apple(42), SerializedValue(Apple(42)).parse[Apple].get)
  }

  @Test
  def serializedValueHasTag(): Unit = {
    val serialized = SerializedValue(Apple(42))
    assertTrue("apple has the apple tag", serialized.hasTag[Apple])
    assertFalse("apple does not have the orange tag", serialized.hasTag[Orange])
    val serializedFruit = SerializedValue[Fruit](Apple(42))
    assertTrue("as-fruit apple has the apple tag", serializedFruit.hasTag[Apple])
    val serializedOrange = SerializedValue(Orange("hello"))
    assertFalse("orange is not tagged as apple", serializedOrange.hasTag[Apple])
  }
}
