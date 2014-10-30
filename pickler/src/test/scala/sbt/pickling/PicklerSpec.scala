package sbt.pickling

import collection.immutable.::
import org.specs2._
import scala.pickling._, sbt.pickling.json._

class PicklerSpec extends Specification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check custom JSON pickling.

  1 should
    pickle as 1                                                 ${ 1.pickle.value must_== "1" }
    and unpickle from 1.                                        ${ "1".unpickle[Int] must_== 1 }
  1L should
    pickle as 1                                                 ${ 1L.pickle.value must_== "1" }
    and unpickle from 1.                                        ${ "1".unpickle[Long] must_== 1L }
  "a" should
    pickle as "a"                                               ${ "a".pickle.value must_== "\"a\"" }
    and unpickle from "a".                                      ${ "\"a\"".unpickle[String] must_== "a" }
  false should
    pickle as false                                             ${ false.pickle.value must_== "false" }
    and unpickle from false.                                    ${ "false".unpickle[Boolean] must_== false }
  1.0 should
    pickle as 1.0                                               ${ 1.0.pickle.value must_== "1.0" }
    and unpickle from 1.0.                                      ${ "1.0".unpickle[Double] must_== 1.0 }
  Array(1: Byte) should
    pickle as [1]                                               ${ pointedByte1[Array]{ _.asInstanceOf[Array[Byte]].pickle.value } }
    and unpickle from [1].                                      ${ pointedByte2[Array]{ _.unpickle[Array[Byte]] } }
  Array(1: Short) should
    pickle as [1]                                               ${ pointedShort1[Array]{ _.asInstanceOf[Array[Short]].pickle.value } }
    and unpickle from [1].                                      ${ pointedShort2[Array]{ _.unpickle[Array[Short]] } }
  Array('a') should
    pickle as ["a"]                                             ${ pointedChar1[Array]{ _.asInstanceOf[Array[Char]].pickle.value } }
    and unpickle from ["a"].                                    ${ pointedChar2[Array]{ _.unpickle[Array[Char]] } }
  Array(1) should
    pickle as [1]                                               ${ pointedInt1[Array]{ _.asInstanceOf[Array[Int]].pickle.value } }
    and unpickle from [1].                                      ${ pointedInt2[Array]{ _.unpickle[Array[Int]] } }
  Array(1L) should
    pickle as [1]                                               ${ pointedLong1[Array]{ _.asInstanceOf[Array[Long]].pickle.value } }
    and unpickle from [1].                                      ${ pointedLong2[Array]{ _.unpickle[Array[Long]] } }
  Array(false) should
    pickle as [false]                                           ${ pointedBoolean1[Array]{ _.asInstanceOf[Array[Boolean]].pickle.value } }
    and unpickle from [false].                                  ${ pointedBoolean2[Array]{ _.unpickle[Array[Boolean]] } }
  Array(1.0F) should
    pickle as [1.0]                                             ${ pointedFloat1[Array]{ _.asInstanceOf[Array[Float]].pickle.value } }
    and unpickle from [1.0].                                    ${ pointedFloat2[Array]{ _.unpickle[Array[Float]] } }
  Array(1.0) should
    pickle as [1.0]                                             ${ pointedDouble1[Array]{ _.asInstanceOf[Array[Double]].pickle.value } }
    and unpickle from [1.0].                                    ${ pointedDouble2[Array]{ _.unpickle[Array[Double]] } }
  List(1: Byte) should
    pickle as [1]                                               ${ pointedByte1[::]{ _.asInstanceOf[::[Byte]].pickle.value } }
    and unpickle from [1].                                      ${ pointedByte2[::]{ _.unpickle[::[Byte]] } }
  List(1: Short) should
    pickle as [1]                                               ${ pointedShort1[::]{ _.asInstanceOf[::[Short]].pickle.value } }
    and unpickle from [1].                                      ${ pointedShort2[::]{ _.unpickle[::[Short]] } }
  List('a') should
    pickle as ["a"]                                             ${ pointedChar1[::]{ _.asInstanceOf[::[Char]].pickle.value } }
    and unpickle from ["a"].                                    ${ pointedChar2[::]{ _.unpickle[::[Char]] } }
  List(1) should
    pickle as [1]                                               ${ pointedInt1[::]{ _.asInstanceOf[::[Int]].pickle.value } }
    and unpickle from [1].                                      ${ pointedInt2[::]{ _.unpickle[::[Int]] } }
  List(1L) should
    pickle as [1]                                               ${ pointedLong1[::]{ _.asInstanceOf[::[Long]].pickle.value } }
    and unpickle from [1].                                      ${ pointedLong2[::]{ _.unpickle[::[Long]] } }
  List(false) should
    pickle as [false]                                           ${ pointedBoolean1[::]{ _.asInstanceOf[::[Boolean]].pickle.value } }
    and unpickle from [false].                                  ${ pointedBoolean2[::]{ _.unpickle[::[Boolean]] } }
  List(1.0F) should
    pickle as [1.0]                                             ${ pointedFloat1[::]{ _.asInstanceOf[::[Float]].pickle.value } }
    and unpickle from [1.0].                                    ${ pointedFloat2[::]{ _.unpickle[::[Float]] } }
  List(1.0) should
    pickle as [1.0]                                             ${ pointedDouble1[::]{ _.asInstanceOf[::[Double]].pickle.value } }
    and unpickle from [1.0].                                    ${ pointedDouble2[::]{ _.unpickle[::[Double]] } }
  Vector(1: Byte) should
    pickle as [1]                                               ${ pointedByte1[Vector]{ _.asInstanceOf[Vector[Byte]].pickle.value } }
    and unpickle from [1].                                      ${ pointedByte2[Vector]{ _.unpickle[Vector[Byte]] } }
  Vector(1: Short) should
    pickle as [1]                                               ${ pointedShort1[Vector]{ _.asInstanceOf[Vector[Short]].pickle.value } }
    and unpickle from [1].                                      ${ pointedShort2[Vector]{ _.unpickle[Vector[Short]] } }
  Vector('a') should
    pickle as ["a"]                                             ${ pointedChar1[Vector]{ _.asInstanceOf[Vector[Char]].pickle.value } }
    and unpickle from ["a"].                                    ${ pointedChar2[Vector]{ _.unpickle[Vector[Char]] } }
  Vector(1) should
    pickle as [1]                                               ${ pointedInt1[Vector]{ _.asInstanceOf[Vector[Int]].pickle.value } }
    and unpickle from [1].                                      ${ pointedInt2[Vector]{ _.unpickle[Vector[Int]] } }
  Vector(1L) should
    pickle as [1]                                               ${ pointedLong1[Vector]{ _.asInstanceOf[Vector[Long]].pickle.value } }
    and unpickle from [1].                                      ${ pointedLong2[Vector]{ _.unpickle[Vector[Long]] } }
  Vector(false) should
    pickle as [false]                                           ${ pointedBoolean1[Vector]{ _.asInstanceOf[Vector[Boolean]].pickle.value } }
    and unpickle from [false].                                  ${ pointedBoolean2[Vector]{ _.unpickle[Vector[Boolean]] } }
  Vector(1.0F) should
    pickle as [1.0]                                             ${ pointedFloat1[Vector]{ _.asInstanceOf[Vector[Float]].pickle.value } }
    and unpickle from [1.0].                                    ${ pointedFloat2[Vector]{ _.unpickle[Vector[Float]] } }
  Vector(1.0) should
    pickle as [1.0]                                             ${ pointedDouble1[Vector]{ _.asInstanceOf[Vector[Double]].pickle.value } }
    and unpickle from [1.0].                                    ${ pointedDouble2[Vector]{ _.unpickle[Vector[Double]] } }
                                                                """
  
  lazy val arrayIntExample = """[
1
]"""
  lazy val arrayCharExample = """[
"a"
]"""
  lazy val arrayBooleanExample = """[
false
]"""
  lazy val arrayDoubleExample = """[
1.0
]"""

  def trimLine(s: String): String =
    (s.lines map {_.trim}).mkString("\n")
  def pointedByte1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed(1: Byte))) must_== arrayIntExample
  }
  def pointedByte2[F[_]: Pointed](f: String => F[Byte]) = {
    val m = implicitly[Pointed[F]]
    f(arrayIntExample) must_== m.pointed(1: Byte)
  }
  def pointedShort1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed(1: Short))) must_== arrayIntExample
  }
  def pointedShort2[F[_]: Pointed](f: String => F[Short]) = {
    val m = implicitly[Pointed[F]]
    f(arrayIntExample) must_== m.pointed(1: Short)
  }
  def pointedChar1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed('a'))) must_== arrayCharExample
  }
  def pointedChar2[F[_]: Pointed](f: String => F[Char]) = {
    val m = implicitly[Pointed[F]]
    f(arrayCharExample) must_== m.pointed('a')
  }
  def pointedInt1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed(1))) must_== arrayIntExample
  }
  def pointedInt2[F[_]: Pointed](f: String => F[Int]) = {
    val m = implicitly[Pointed[F]]
    f(arrayIntExample) must_== m.pointed(1)
  }
  def pointedLong1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed(1L))) must_== arrayIntExample
  }
  def pointedLong2[F[_]: Pointed](f: String => F[Long]) = {
    val m = implicitly[Pointed[F]]
    f(arrayIntExample) must_== m.pointed(1L)
  }
  def pointedBoolean1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed(false))) must_== arrayBooleanExample
  }
  def pointedBoolean2[F[_]: Pointed](f: String => F[Boolean]) = {
    val m = implicitly[Pointed[F]]
    f(arrayBooleanExample) must_== m.pointed(false)
  }
  def pointedFloat1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed(1.0F))) must_== arrayDoubleExample
  }
  def pointedFloat2[F[_]: Pointed](f: String => F[Float]) = {
    val m = implicitly[Pointed[F]]
    f(arrayDoubleExample) must_== m.pointed(1.0F)
  }
  def pointedDouble1[F[_]: Pointed](f: Any => String) = {
    val m = implicitly[Pointed[F]]
    trimLine(f(m.pointed(1.0))) must_== arrayDoubleExample
  }
  def pointedDouble2[F[_]: Pointed](f: String => F[Double]) = {
    val m = implicitly[Pointed[F]]
    f(arrayDoubleExample) must_== m.pointed(1.0)
  }
}
