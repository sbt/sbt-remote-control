package sbt.protocol.spec

import sbt.protocol
import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import sbt.serialization._
import JUnitUtil._

class TypeExpressionPicklerTest {

  @Test
  def testRoundtripStringType: Unit = {
    val value = TypeExpression.parse("java.lang.String")._1
    val example = "\"java.lang.String\""
    SerializedValue(value).toJsonString must_== example
    roundTrip(value)
  }

  @Test
  def testRoundtripListOfStringType: Unit = {
    val value = TypeExpression.parse("scala.collection.immutable.List[java.lang.String]")._1
    val example = "\"scala.collection.immutable.List[java.lang.String]\""
    SerializedValue(value).toJsonString must_== example
    roundTrip(value)
  }

  val key = protocol.AttributeKey("name", TypeExpression.parse("java.lang.String")._1)
  val build = new java.net.URI("file:///test/project")
  val projectRef = protocol.ProjectReference(build, "test")
  val scope = protocol.SbtScope(project = Some(projectRef))
  val scopedKey = protocol.ScopedKey(key, scope)
  val seqAttributedFile = Seq(
    protocol.Attributed(new java.io.File("/some.file")))

  @Test
  def testRoundtripAttributeKey: Unit = {
    roundTrip(key)
  }

  @Test
  def testRoundtripScope: Unit = {
    roundTrip(scope)
  }

  @Test
  def testRoundtripScopedKey: Unit = {
    roundTrip(scopedKey)
  }

  @Test
  def testRoundtripSeqAttributedFile: Unit = {
    roundTrip(seqAttributedFile)
  }
}
