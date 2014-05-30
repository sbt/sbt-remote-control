/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
package com.typesafe.sbtrc.controller

import org.junit.Assert._
import org.junit._

class VersionCompareTest {

  @Test
  def testVersionCompare(): Unit = {
    // all these are in ascending order
    val tests = Seq("1" -> "2",
      "1.2" -> "1.3",
      "1.2" -> "1.2.1",
      "1.2-RC1" -> "1.2",
      "2.3.0-RC2" -> "2.3.0",
      "2.3.0-M1" -> "2.3.0",
      "2.3.0-RC1" -> "2.3.0-RC2",
      "2.3.0-M3" -> "2.3.0-M5",
      "2.3.0" -> "2.3.1-RC1",
      "2.3.0-M3" -> "2.3.0-M34")
    def check(t: (String, String), expected: Int): Unit = {
      val result = VersionCompare(t._1, t._2) match {
        // clamp to 1 or -1 if the compare returned something outside
        case n if n <= -1 => -1
        case n if n >= 1 => 1
        case n => n
      }
      assertEquals(s"${t._1} compare ${t._2} == ${expected}",
        expected, result)
    }
    for (t <- tests) {
      check(t, -1)
      check(t._2 -> t._1, 1)
      check(t._1 -> t._1, 0)
      check(t._2 -> t._2, 0)
    }
  }
}
