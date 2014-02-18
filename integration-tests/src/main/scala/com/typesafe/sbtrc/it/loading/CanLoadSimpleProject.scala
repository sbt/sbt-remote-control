package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._

import concurrent.duration.Duration.Inf
import concurrent.Await

class CanLoadSimpleProject extends SbtClientTest {
  // TODO - Don't hardcode sbt versions, unless we have to...
  val dummy = utils.makeDummySbtProject("test", "0.13.1")

  withSbt(dummy) { client =>
    val build = concurrent.promise[MinimalBuildStructure]
    import concurrent.ExecutionContext.Implicits.global
    client watchBuild build.success
    // We do this for debugging only.
    client handleEvents { event => println(event) }

    println("About to wait for build result...")
    val result = Await.result(build.future, defaultTimeout)
    println("Have result!...")
    assert(result.projects.size == 1, "Found too many projects!")
    val project = result.projects.head
    assert(project.name == "test", "failed to discover project name == file name.")
    println("Test is complete!")
  }

}