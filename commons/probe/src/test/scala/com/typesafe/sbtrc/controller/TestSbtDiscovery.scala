package com.typesafe.sbtrc


import org.junit.Assert._
import org.junit._
import SbtDiscovery._

class TestSbtDiscovery {
  
  @Test
  def canSortOfDetermineInputTaskKeys(): Unit = {
    val name = sbt.Keys.name in sbt.Compile
    val sources = sbt.Keys.sources in sbt.Compile
    val run = sbt.Keys.run in sbt.Compile
    assertFalse("Could not figure out `name` is not an input task!", isInputKey(name.scopedKey))
    assertFalse("Could not figure out spirces is not an input task!", isInputKey(sources))
    assertTrue("Could not figure out run is an input task!", isInputKey(run))
  }
  
  @Test
  def canSortOfDetermineTaskKeys(): Unit = {
    val name = sbt.Keys.name in sbt.Compile
    val sources = sbt.Keys.sources in sbt.Compile
    val run = sbt.Keys.run in sbt.Compile
    assertFalse("Could not figure out `name` is not a task!", isInputKey(name.scopedKey))
    assertTrue("Could not figure out `sources` is a task!", isTaskKey(sources))
    assertFalse("Could not figure out `run` is not a task!", isTaskKey(run))
  }
}