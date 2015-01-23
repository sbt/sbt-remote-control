package sbt.protocol.spec

import sbt.protocol
import org.junit.Assert._
import org.junit._
import java.io.File
import java.net.URI
import scala.pickling.Defaults.pickleOps
import sbt.serialization._, json._
import sbt.serialization.spec.JUnitUtil._
import SpecsUtil._
import scala.pickling.internal.AppliedType

class AppliedTypePicklerTest {
  import protocol.CoreProtocol._

  @Test
  def testRoundtripStringType: Unit = {
    val value = AppliedType.parse("java.lang.String")._1
    val example = "\"java.lang.String\""
    value.pickle.value must_== example
    roundTrip(value)
  }

  @Test
  def testRoundtripListOfStringType: Unit = {
    val value = AppliedType.parse("scala.collection.immutable.List[java.lang.String]")._1
    val example = "\"scala.collection.immutable.List[java.lang.String]\""
    value.pickle.value must_== example
    roundTrip(value)
  }

  val key = protocol.AttributeKey("name", AppliedType.parse("java.lang.String")._1)
  val build = new java.net.URI("file:///test/project")
  val projectRef = protocol.ProjectReference(build, "test")
  val scope = protocol.SbtScope(project = Some(projectRef))
  val scopedKey = protocol.ScopedKey(key, scope)

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
}
