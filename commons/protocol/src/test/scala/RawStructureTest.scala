package com.typesafe.sbtrc
package controller

import protocol._
import org.junit.Assert._
import org.junit._


class RawStructureTest {
  @Test
  def canSerializeSeqString(): Unit = {
    val value = Seq("hi", "byte")
    val expected = BuildValue(value)
    // We can serialize sequence of strings.
    assertTrue("Should be able to serialize a Seq[String]", expected.value.isDefined)
    
    val result = JsonStructure.unapply[BuildValue[Seq[String]]](JsonStructure(expected))
    assertTrue("Should be able to deserialize any BuildValue.", result.isDefined)
    val rawResult = result.get
    assertEquals("Failed to serailize/deserialize build value [seq[string]] correctly", expected.value, rawResult.value)
  }
}