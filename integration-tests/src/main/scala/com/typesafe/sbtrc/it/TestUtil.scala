package com.typesafe.sbtrc

import java.io.File

final class TestUtil(val scratchDir: File) {
  import TestUtil.defaultSbtTestVersion

  def createFile(name: java.io.File, content: String): Unit = {
    val writer = new java.io.FileWriter(name)
    try writer.write(content)
    finally writer.close()
  }

  def makeDummyEmptyDirectory(relativeDir: String): File = {
    val dir = new File(scratchDir, relativeDir)
    if (!dir.isDirectory()) dir.mkdirs()
    dir
  }

  def makeEmptySbtProject(relativeDir: String, sbtVersion: String = defaultSbtTestVersion): File = {
    val dir = makeDummyEmptyDirectory(relativeDir)

    val project = new File(dir, "project")
    if (!project.isDirectory()) project.mkdirs()

    val props = new File(project, "build.properties")
    createFile(props, "sbt.version=" + sbtVersion)

    dir
  }

  /** Creates a dummy project we can run sbt against. */
  def makeDummySbtProject(relativeDir: String, sbtVersion: String = defaultSbtTestVersion): File = {
    val dir = makeEmptySbtProject(relativeDir, sbtVersion)

    val build = new File(dir, "build.sbt")
    createFile(build, s"""
name := "${relativeDir}"

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test"
""")

    val scalaSource = new File(dir, "src/main/scala")
    if (!scalaSource.isDirectory()) scalaSource.mkdirs()
    val main = new File(scalaSource, "hello.scala")
    createFile(main, "object Main extends App { println(\"Hello World\") }\n")

    val testSource = new File(dir, "src/test/scala")
    if (!testSource.isDirectory()) testSource.mkdirs()
    val tests = new File(testSource, "tests.scala")
    createFile(tests, """
import org.junit.Assert._
import org.junit._

class OnePassOneFailTest {
    @Test
    def testThatShouldPass: Unit = {
    }

    @Test
    def testThatShouldFail: Unit = {
        assertTrue("this is not true", false)
    }
}

class OnePassTest {
    @Test
    def testThatShouldPass: Unit = {
    }
}

class OneFailTest {
    @Test
    def testThatShouldFail: Unit = {
        assertTrue("this is not true", false)
    }
}
""")

    dir
  }

  def makeDummySbtProjectWithBrokenBuild(relativeDir: String, sbtVersion: String = defaultSbtTestVersion): File = {
    val dir = makeDummySbtProject(relativeDir, sbtVersion)

    val build = new File(dir, "build.sbt")
    createFile(build, "BLARG := \"" + relativeDir + "\"\n")

    dir
  }

  def makeDummySbtProjectWithNoMain(relativeDir: String, sbtVersion: String = defaultSbtTestVersion): File = {
    val dir = makeDummySbtProject(relativeDir, sbtVersion)

    val main = new File(dir, "src/main/scala/hello.scala")
    // doesn't extend App
    createFile(main, "object Main { println(\"Hello World\") }\n")

    dir
  }

  def makeDummySbtProjectWithMultipleMain(relativeDir: String, sbtVersion: String = defaultSbtTestVersion): File = {
    val dir = makeDummySbtProject(relativeDir, sbtVersion)

    val main = new File(dir, "src/main/scala/hello.scala")
    createFile(main, """
object Main1 extends App { println("Hello World 1") }
object Main2 extends App { println("Hello World 2") }
object Main3 extends App { println("Hello World 3") }
""")

    dir
  }
}

object TestUtil {
  private val sbtVersionUsedToCompileTests = properties.SbtRcProperties.SBT_VERSION
  val sbt12TestVersion = "0.12.4"
  val sbt13TestVersion = "0.13.0"
  def defaultSbtTestVersion = sbt13TestVersion

  if (!(sbtVersionUsedToCompileTests == sbt12TestVersion || sbtVersionUsedToCompileTests == sbt13TestVersion))
    throw new RuntimeException("One of the versions we use to test should probably be the one we build with just to save downloads")
}
