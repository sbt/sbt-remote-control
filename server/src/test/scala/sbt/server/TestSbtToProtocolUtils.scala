package sbt.server

import org.junit.Assert._
import org.junit._
import sbt.protocol
import sbt.ConfigKey.configurationToKey
import sbt.Scoped.inputScopedToKey
import sbt.Scoped.taskScopedToKey
import sbt.serialization.AppliedType

class TestSbtToProtocolUtils {

  @Test
  def testProjectScopeMapping(): Unit = {
    val ref = sbt.ProjectRef(
      new java.net.URI("file:///home/"), "test-project")
    val nameInRef = sbt.Keys.name in ref
    val string = AppliedType("java.lang.String", Nil)
    val expectedKey =
      protocol.AttributeKey("name", string)
    val expectedScope =
      protocol.SbtScope(
        project = Some(protocol.ProjectReference(
          new java.net.URI("file:///home/"),
          "test-project")),
        build = Some(new java.net.URI("file:///home/")))
    val expected = protocol.ScopedKey(expectedKey, expectedScope)
    val actual = SbtToProtocolUtils.scopedKeyToProtocol(nameInRef.scopedKey)
    assertEquals("Failed to convert sbt key:", expected, actual)
  }

  @Test
  def testKeyMapping(): Unit = {
    val sbtName = sbt.Keys.name
    val string = AppliedType("java.lang.String", Nil)
    val expected =
      protocol.AttributeKey("name", string)

    val actual = SbtToProtocolUtils.keyToProtocol(sbtName.key)
    assertEquals("Failed to covnert sbt key:", expected, actual)
  }

  @Test
  def testScopedInputKeyMapping(): Unit = {
    val sbtRunInCompile = sbt.Keys.run in sbt.Compile
    val unit = AppliedType("void", Nil)
    //val inputTaskType = AppliedType("sbt.InputTask", Seq(unit))

    val runInputKey =
      protocol.AttributeKey("run", unit)
    val scope = protocol.SbtScope(config = Some("compile"))
    val expected = protocol.ScopedKey(runInputKey, scope)

    val actual = SbtToProtocolUtils.scopedKeyToProtocol(sbtRunInCompile)
    assertEquals("Failed to covnert sbt key:", expected, actual)
  }

  @Test
  def testScopedTaskKeyMapping(): Unit = {
    // Here we ensure the Task[_] part is stripped from the key in the remote API since we cannot (nor should)
    // serialize tasks.
    val sbtSourcesInCompile = sbt.Keys.sources in sbt.Compile
    val file = AppliedType("java.io.File", Nil)
    val seqFile = AppliedType("scala.collection.Seq", List(file))
    val sourcesTaskKey =
      protocol.AttributeKey("sources", seqFile)
    val scope = protocol.SbtScope(config = Some("compile"))
    val expected = protocol.ScopedKey(sourcesTaskKey, scope)

    val actual = SbtToProtocolUtils.scopedKeyToProtocol(sbtSourcesInCompile)
    assertEquals("Failed to covnert sbt key:", expected, actual)
  }
}
