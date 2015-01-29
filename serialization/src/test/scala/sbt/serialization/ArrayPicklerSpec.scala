package sbt.serialization.spec

import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import JUnitUtil._
import sbt.serialization._
import scala.language.higherKinds

class ArrayPicklerTest {

  @Test
  def testArrays: Unit = {
    // Array(1: Byte) should pickle as [1]
    pointed1[Array, Byte]
    // pointed2[Array, Byte]

    // Array(1: Short) should pickle as [1]
    pointed1[Array, Short]
    // pointed2[Array, Short]

    // Array('a') should pickle as ["a"].
    pointed1[Array, Char]
    // pointed2[Array, Char]

    // Array("a") should pickle as ["a"]
    pointed1[Array, String]
    // pointed2[Array, String]

    // Array(1) should pickle as [1]
    pointed1[Array, Int]
    // pointed2[Array, Int]

    // Array(1L) should pickle as [1]
    pointed1[Array, Long]
    // pointed2[Array, Long]

    // Array(false) should pickle as [false]
    pointed1[Array, Boolean]
    // pointed2[Array, Boolean]

    // Array(1.0F) should pickle as [1.0]
    pointed1[Array, Float]
    // pointed2[Array, Float]

    // Array(1.0) should pickle as [1.0]
    pointed1[Array, Double]
    // pointed2[Array, Double]
  }

  @Test
  def testLists: Unit = {
    // List(1: Byte) should pickle as [1]
    pointed1[List, Byte]
    pointed2[List, Byte]
    // List(1: Short) should pickle as [1]
    pointed1[List, Short]
    pointed2[List, Short]
    // List('a') should pickle as ["a"]
    pointed1[List, Char]
    pointed2[List, Char]
    // List("a") should pickle as ["a"]
    pointed1[List, String]
    pointed2[List, String]
    // List(1) should pickle as [1]
    pointed1[List, Int]
    pointed2[List, Int]
    // List(1L) should pickle as [1]
    pointed1[List, Long]
    pointed2[List, Long]
    // List(false) should pickle as [false]
    pointed1[List, Boolean]
    pointed2[List, Boolean]
    // List(1.0F) should pickle as [1.0]
    pointed1[List, Float]
    pointed2[List, Float]
    // List(1.0) should pickle as [1.0]
    pointed1[List, Double]
    pointed2[List, Double]
  }

  @Test
  def testVectors: Unit = {
    // Vector(1: Byte) should pickle as [1]
    pointed1[Vector, Byte]
    pointed2[Vector, Byte]
    // Vector(1: Short) should pickle as [1]
    pointed1[Vector, Short]
    pointed2[Vector, Short]
    // Vector('a') should pickle as ["a"]
    pointed1[Vector, Char]
    pointed2[Vector, Char]
    // Vector("a") should pickle as ["a"]
    pointed1[Vector, String]
    pointed2[Vector, String]
    // Vector(1) should pickle as [1]
    pointed1[Vector, Int]
    pointed2[Vector, Int]
    // Vector(1L) should pickle as [1]
    pointed1[Vector, Long]
    pointed2[Vector, Long]
    // Vector(false) should pickle as [false]
    pointed1[Vector, Boolean]
    pointed2[Vector, Boolean]
    // Vector(1.0F) should pickle as [1.0]
    pointed1[Vector, Float]
    pointed2[Vector, Float]
    // Vector(1.0) should pickle as [1.0]
    pointed1[Vector, Double]
    pointed2[Vector, Double]
  }

  @Test
  def testOptions: Unit = {
    SerializedValue(Some(1): Option[Int]).toJsonString must_== "1"
    SerializedValue.fromJsonString("1").parse[Option[Int]].get must_== Some(1)

    SerializedValue(Some("a"): Option[String]).toJsonString must_== "\"a\""
    SerializedValue.fromJsonString("\"a\"").parse[Option[String]].get must_== Some("a")

    SerializedValue(None: Option[Int]).toJsonString must_== "null"
    SerializedValue.fromJsonString("null").parse[Option[Int]].get must_== None

    SerializedValue(None: Option[String]).toJsonString must_== "null"
    SerializedValue.fromJsonString("null").parse[Option[String]].get must_== None
  }

  @Test
  def testRoundtrip: Unit = {
    // TODO it would be nice to pickle Nil.type so this works
    //roundTrip(Nil) // custom format to support both Nil and List[A]
    roundTrip(Nil: List[String])
    roundTrip(Vector(): Vector[String])
    roundTripArray(Array(1, 2, 3))
    roundTripArray(Array("Bar", "Baz"))
    roundTrip(Vector("Bar", "Baz"))
    roundTrip(List("Bar", "Baz"))
    roundTrip(Vector(1, 2, 3))
  }

  def trimLine(s: String): String =
    (s.lines map { _.trim }).mkString("\n")
  def pointed1[F[_], A: ClassManifest](implicit m: Pointed[F], ae: ArrayExample[A], ev0: SPickler[F[A]], ev1: FastTypeTag[F[A]]) =
    assertEquals(s"With type $ev1", ae.arrayJson, (trimLine(SerializedValue(m.pointed(ae.one)).toJsonString)))
  def pointed2[F[_], A: ClassManifest](implicit m: Pointed[F], ae: ArrayExample[A], ev0: Unpickler[F[A]], ev1: FastTypeTag[F[A]]) =
    SerializedValue.fromJsonString(ae.arrayJson).parse[F[A]].get must_== m.pointed(ae.one)
}

trait ArrayExample[A] {
  def one: A
  def arrayJson: String
}
object ArrayExample {
  def apply[A](one0: A, arrayJson0: String) = new ArrayExample[A] {
    def one = one0
    def arrayJson: String = arrayJson0
  }
  val arrayIntExample = """[1]"""
  val arrayDoubleExample = """[1.0]"""
  val arrayStringExample = """["a"]"""
  implicit val byteArrayExample: ArrayExample[Byte] = ArrayExample(1: Byte, arrayIntExample)
  implicit val shortArrayExample: ArrayExample[Short] = ArrayExample(1: Short, arrayIntExample)
  implicit val intArrayExample: ArrayExample[Int] = ArrayExample(1, arrayIntExample)
  implicit val charArrayExample: ArrayExample[Char] = ArrayExample('a', arrayStringExample)
  implicit val stringArrayExample: ArrayExample[String] = ArrayExample("a", arrayStringExample)
  implicit val longArrayExample: ArrayExample[Long] = ArrayExample(1L, arrayIntExample)
  implicit val booleanArrayExample: ArrayExample[Boolean] = ArrayExample(false, """[false]""".stripMargin)
  implicit val floatArrayExample: ArrayExample[Float] = ArrayExample(1.0F, arrayDoubleExample)
  implicit val doubleArrayExample: ArrayExample[Double] = ArrayExample(1.0, arrayDoubleExample)
}
