package com.typesafe.sbtrc
package it
package loading

import scala.concurrent.Promise
import sbt.client._

class CanLoadProjectWithPlugin extends SbtClientTest {
  val dummy = utils.makeDummySbtProject("pluginTest")
  sbt.IO.write(
    new java.io.File(dummy, "project/plugins.sbt"),
    """addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")""".stripMargin)

  withSbt(dummy) { client =>
    val build = Promise[MinimalBuildStructure]
    import concurrent.ExecutionContext.Implicits.global
    client watchBuild build.trySuccess
    val result = waitWithError(build.future, "Never got build structure.")
    assert(result.projects.size == 1, "Found too many projects!")
    val project = result.projects.head
    assert(project.id.name == "pluginTest", "failed to discover project name == file name.")
    assert(project.plugins contains "com.typesafe.sbteclipse", s"failed to discover default plugins in project, found: ${project.plugins.mkString(", ")}")
  }
}
