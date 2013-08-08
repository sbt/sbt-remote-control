package com.typesafe.sbtrc

import org.junit.Assert._
import org.junit._


class TestSbtToProtocolUtils {
  
  @Test
  def testProjectScopeMapping(): Unit = {
    val ref = sbt.ProjectRef(
        new java.net.URI("file:///home/"), "test-project")
    val nameInRef = sbt.Keys.name in ref
    val string = protocol.TypeInfo("java.lang.String")
    val expectedKey = 
      protocol.AttributeKey("name", string)
    val expectedScope =
      protocol.SbtScope(
        project = Some(protocol.ProjectReference(
          new java.net.URI("file:///home/"),
          "test-project"    
        ))    
      )
    val expected = protocol.ScopedKey(expectedKey, expectedScope)
    val actual = SbtToProtocolUtils.scopedKeyToProtocol(nameInRef.scopedKey)
    assertEquals("Failed to covnert sbt key:", expected, actual)
  }
  
  @Test
  def testKeyMapping(): Unit = {
    val sbtName = sbt.Keys.name
    val string = protocol.TypeInfo("java.lang.String")
    val expected = 
      protocol.AttributeKey("name", string)
    
    val actual = SbtToProtocolUtils.keyToProtocol(sbtName.key)
    assertEquals("Failed to covnert sbt key:", expected, actual)
  }
  
  
  @Test
  def testScopedInputKeyMapping(): Unit = {
    val sbtRunInCompile = sbt.Keys.run in sbt.Compile
    val unit = protocol.TypeInfo("void")
    val inputTaskType = protocol.TypeInfo("sbt.InputTask", Seq(unit))
    
    val runInputKey = 
      protocol.AttributeKey("run", inputTaskType)
    val scope = protocol.SbtScope(config = Some("compile"))
    val expected = protocol.ScopedKey(runInputKey, scope)
    
    val actual = SbtToProtocolUtils.scopedKeyToProtocol(sbtRunInCompile)
    assertEquals("Failed to covnert sbt key:", expected, actual)
  }
  
  
  @Test
  def testScopedTaskKeyMapping(): Unit = {
    val sbtSourcesInCompile = sbt.Keys.sources in sbt.Compile
    val file = protocol.TypeInfo("java.io.File")
    val seqFile = protocol.TypeInfo("scala.collection.Seq", Seq(file))
    val taskSeqFile = protocol.TypeInfo("sbt.Task", Seq(seqFile))
    val sourcesTaskKey = 
      protocol.AttributeKey("sources", taskSeqFile)
    val scope = protocol.SbtScope(config = Some("compile"))
    val expected = protocol.ScopedKey(sourcesTaskKey, scope)
    
    val actual = SbtToProtocolUtils.scopedKeyToProtocol(sbtSourcesInCompile)
    assertEquals("Failed to covnert sbt key:", expected, actual)
  }
}