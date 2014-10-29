package sbt.pickling


import org.specs2._
import scala.pickling._, sbt.pickling.json._

class PicklerSpec extends Specification {
  def is = s2"""

  This is a specification to check custom JSON pickling.

  1 should
    pickle as 1 ${ 1.pickle.value must_== "1" }
    and unpickle from 1 ${ "1".unpickle[Int] must_== 1 }.
  1L should
    pickle as "1" ${ 1L.pickle.value must_== "\"1\"" }
    and unpickle from "1" ${ "\"1\"".unpickle[Long] must_== 1L }.
  "a" should
    pickle as "a" ${ "a".pickle.value must_== "\"a\"" }
    and unpickle from "a" ${ "\"a\"".unpickle[String] must_== "a" }.
  false should
    pickle as false ${ false.pickle.value must_== "false" }
    and unpickle from false ${ "false".unpickle[Boolean] must_== false }.
  1.0 should
    pickle as 1.0 ${ 1.0.pickle.value must_== "1.0" }
    and unpickle from 1.0 ${ "1.0".unpickle[Double] must_== 1.0 }.    

  Array(1) should
    pickle as [1] ${ Array(1).pickle.value must_== "[\n  1\n]" }
    and unpickle from [1] ${ "[1]".unpickle[Array[Int]] must_== Array(1) }.
  Array(1L) should
    pickle as ["1"] ${ Array(1L).pickle.value must_== "[\n  \"1\"\n]" }
    and unpickle from ["1"] ${ "[\"1\"]".unpickle[Array[Long]] must_== Array(1L) }.
  Array(false) should
    pickle as [false] ${ Array(false).pickle.value must_== "[\n  false\n]" }
    and unpickle from [false] ${ "[false]".unpickle[Array[Boolean]] must_== Array(false) }.
  List(1) should
    pickle as { "$$type": ..., "$$elems": ... } $listInt1
    and unpickle from { "$$type": ..., "$$elems": ... } $listInt2.
                                                                """
  
  val listIntExample = """{
  "$type": "scala.collection.immutable.$colon$colon[scala.Int]",
  "$elems": [
    1
  ]
}"""
  def listInt1 = List(1).pickle.value must_== listIntExample
  def listInt2 = listIntExample.unpickle[List[Int]] must_== List(1)
  
}
