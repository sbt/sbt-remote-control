package sbt.pickling.spec

import org.specs2._
import scala.pickling._, sbt.pickling.json._

class ArrayPicklerSpec extends Specification {
  def is = args(sequential = true) ^ s2"""

  This is a specification to check custom JSON pickling.

  Array(1: Byte) should
    pickle as [1]                                               ${pointed1[Array, Byte]}
    and unpickle from [1].                                      ${pointed2[Array, Byte]}
  Array(1: Short) should
    pickle as [1]                                               ${pointed1[Array, Short]}
    and unpickle from [1].                                      ${pointed2[Array, Short]}
  Array('a') should
    pickle as ["a"]                                             ${pointed1[Array, Char]}
    and unpickle from ["a"].                                    ${pointed2[Array, Char]}
  Array("a") should
    pickle as ["a"]                                             ${pointed1[Array, String]}
    and unpickle from ["a"].                                    ${pointed2[Array, String]}
  Array(1) should
    pickle as [1]                                               ${pointed1[Array, Int]}
    and unpickle from [1].                                      ${pointed2[Array, Int]}
  Array(1L) should
    pickle as [1]                                               ${pointed1[Array, Long]}
    and unpickle from [1].                                      ${pointed2[Array, Long]}
  Array(false) should
    pickle as [false]                                           ${pointed1[Array, Boolean]}
    and unpickle from [false].                                  ${pointed2[Array, Boolean]}
  Array(1.0F) should
    pickle as [1.0]                                             ${pointed1[Array, Float]}
    and unpickle from [1.0].                                    ${pointed2[Array, Float]}
  Array(1.0) should
    pickle as [1.0]                                             ${pointed1[Array, Double]}
    and unpickle from [1.0].                                    ${pointed2[Array, Double]}
  List(1: Byte) should
    pickle as [1]                                               ${pointed1[List, Byte]}
    and unpickle from [1].                                      ${pointed2[List, Byte]}
  List(1: Short) should
    pickle as [1]                                               ${pointed1[List, Short]}
    and unpickle from [1].                                      ${pointed2[List, Short]}
  List('a') should
    pickle as ["a"]                                             ${pointed1[List, Char]}
    and unpickle from ["a"].                                    ${pointed2[List, Char]}
  List("a") should
    pickle as ["a"]                                             ${pointed1[List, String]}
    and unpickle from ["a"].                                    ${pointed2[List, String]}
  List(1) should
    pickle as [1]                                               ${pointed1[List, Int]}
    and unpickle from [1].                                      ${pointed2[List, Int]}
  List(1L) should
    pickle as [1]                                               ${pointed1[List, Long]}
    and unpickle from [1].                                      ${pointed2[List, Long]}
  List(false) should
    pickle as [false]                                           ${pointed1[List, Boolean]}
    and unpickle from [false].                                  ${pointed2[List, Boolean]}
  List(1.0F) should
    pickle as [1.0]                                             ${pointed1[List, Float]}
    and unpickle from [1.0].                                    ${pointed2[List, Float]}
  List(1.0) should
    pickle as [1.0]                                             ${pointed1[List, Double]}
    and unpickle from [1.0].                                    ${pointed2[List, Double]}
  Vector(1: Byte) should
    pickle as [1]                                               ${pointed1[Vector, Byte]}
    and unpickle from [1].                                      ${pointed2[Vector, Byte]}
  Vector(1: Short) should
    pickle as [1]                                               ${pointed1[Vector, Short]}
    and unpickle from [1].                                      ${pointed2[Vector, Short]}
  Vector('a') should
    pickle as ["a"]                                             ${pointed1[Vector, Char]}
    and unpickle from ["a"].                                    ${pointed2[Vector, Char]}
  Vector("a") should
    pickle as ["a"]                                             ${pointed1[Vector, String]}
    and unpickle from ["a"].                                    ${pointed2[Vector, String]}
  Vector(1) should
    pickle as [1]                                               ${pointed1[Vector, Int]}
    and unpickle from [1].                                      ${pointed2[Vector, Int]}
  Vector(1L) should
    pickle as [1]                                               ${pointed1[Vector, Long]}
    and unpickle from [1].                                      ${pointed2[Vector, Long]}
  Vector(false) should
    pickle as [false]                                           ${pointed1[Vector, Boolean]}
    and unpickle from [false].                                  ${pointed2[Vector, Boolean]}
  Vector(1.0F) should
    pickle as [1.0]                                             ${pointed1[Vector, Float]}
    and unpickle from [1.0].                                    ${pointed2[Vector, Float]}
  Vector(1.0) should
    pickle as [1.0]                                             ${pointed1[Vector, Double]}
    and unpickle from [1.0].                                    ${pointed2[Vector, Double]}
  Some(1): Option[Int] should
    pickle as 1                                                 ${ (Some(1): Option[Int]).pickle.value must_== "1" }
    and unpickle from 1.                                        ${ "1".unpickle[Option[Int]] must_== Some(1) }
  Some("a"): Option[String] should
    pickle as "a"                                               ${ (Some("a"): Option[String]).pickle.value must_== "\"a\"" }
    and unpickle from "a".                                      ${ "\"a\"".unpickle[Option[String]] must_== Some("a") }
  None: Option[Int] should
    pickle as null                                              ${ (None: Option[Int]).pickle.value must_== "null" }
    and unpickle from null.                                     ${ "null".unpickle[Option[Int]] must_== None }
  None: Option[String] should
    pickle as null                                              ${ (None: Option[String]).pickle.value must_== "null" }
    and unpickle from null.                                     ${ "null".unpickle[Option[String]] must_== None }
                                                                """

  def trimLine(s: String): String =
    (s.lines map {_.trim}).mkString("\n")
  def pointed1[F[_], A: ClassManifest](implicit m: Pointed[F], ae: ArrayExample[A], ev0: SPickler[F[A]], ev1: FastTypeTag[F[A]]) =
    trimLine(m.pointed(ae.one).pickle.value) must_== ae.arrayJson
  def pointed2[F[_], A: ClassManifest](implicit m: Pointed[F], ae: ArrayExample[A], ev0: Unpickler[F[A]], ev1: FastTypeTag[F[A]]) =
    ae.arrayJson.unpickle[F[A]] must_== m.pointed(ae.one)
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
  val arrayIntExample = """[
    |1
    |]""".stripMargin
  val arrayDoubleExample = """[
    |1.0
    |]""".stripMargin
  val arrayStringExample = """[
    |"a"
    |]""".stripMargin
  implicit val byteArrayExample: ArrayExample[Byte] = ArrayExample(1: Byte, arrayIntExample)
  implicit val shortArrayExample: ArrayExample[Short] = ArrayExample(1: Short, arrayIntExample)
  implicit val intArrayExample: ArrayExample[Int] = ArrayExample(1, arrayIntExample)
  implicit val charArrayExample: ArrayExample[Char] = ArrayExample('a', arrayStringExample)
  implicit val stringArrayExample: ArrayExample[String] = ArrayExample("a", arrayStringExample)
  implicit val longArrayExample: ArrayExample[Long] = ArrayExample(1L, arrayIntExample)
  implicit val booleanArrayExample: ArrayExample[Boolean] = ArrayExample(false, """[
    |false
    |]""".stripMargin)
  implicit val floatArrayExample: ArrayExample[Float] = ArrayExample(1.0F, arrayDoubleExample)
  implicit val doubleArrayExample: ArrayExample[Double] = ArrayExample(1.0, arrayDoubleExample)
}
