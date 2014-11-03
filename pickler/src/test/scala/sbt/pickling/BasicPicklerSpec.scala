package sbt.pickling.spec

import collection.immutable.::
import org.specs2._
import scala.pickling._, sbt.pickling.json._

class BasicPicklerSpec extends Specification {
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
                                                                """
}
