package sbt.serialization.spec

import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import scala.pickling.{ PickleOps, UnpickleOps }
import sbt.serialization._, sbt.serialization.json._
import JUnitUtil._
import sbt.serialization.pickler.{
  PrimitivePicklers,
  PrimitiveArrayPicklers,
  CanToStringPicklers,
  OptionPicklers,
  ThrowablePicklers
}

import scala.pickling.ops._
import scala.pickling.static._

class BasicPicklerTest {
  val basicProtocol = new PrimitivePicklers with PrimitiveArrayPicklers with CanToStringPicklers with OptionPicklers with ThrowablePicklers {
    implicit val staticOnly = scala.pickling.static.StaticOnly
  }
  import basicProtocol._

  @Test
  def testInt: Unit = {
    1.pickle.value must_== "1"
    "1".unpickle[Int] must_== 1
  }

  @Test
  def testLong: Unit = {
    1L.pickle.value must_== "1"
    "1".unpickle[Long] must_== 1L
  }

  @Test
  def testString: Unit = {
    "a".pickle.value must_== "\"a\""
    "\"a\"".unpickle[String] must_== "a"
  }

  @Test
  def testBoolean: Unit = {
    false.pickle.value must_== "false"
    "false".unpickle[Boolean] must_== false
  }

  @Test
  def testDouble: Unit = {
    1.0.pickle.value must_== "1.0"
    "1.0".unpickle[Double] must_== 1.0
  }

  @Test
  def testRoundtrip: Unit = {
    roundTrip("Foo")
    roundTrip(new File("/tmp"))
    roundTrip(new URI("/tmp"))
    roundTrip(true)
    roundTrip(false)
    roundTrip(10: Short)
    roundTrip(11)
    roundTrip(12L)
    roundTrip(13.0f)
    roundTrip(14.0)
    roundTrip(None: Option[String]) // roundTrip(None) must fail to compile
    roundTrip(Some("Foo"): Option[String]) // roundTrip(Some("Foo")) must fail to compile
    roundTrip(Some(true): Option[Boolean]) // roundTrip(Some(true)) must fail to compile
    roundTrip(Some(10): Option[Int]) // roundTrip(Some(10)) must fail to compile
  }

  @Test
  def testThrowable: Unit = {
    roundTrip(new Exception(): Throwable)
    roundTrip(new Exception("foo"): Throwable)
    val nested: Throwable = new Exception("foo", new Exception("bar"))
    val recovered = nested.pickle.value.unpickle[Throwable]
    recovered.getCause.getMessage must_== "bar"
    roundTrip(nested)
    recovered.getStackTrace()(0).getFileName must_== "BasicPicklerSpec.scala"
  }
}
