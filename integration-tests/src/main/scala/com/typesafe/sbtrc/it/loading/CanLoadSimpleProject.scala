package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._

import concurrent.duration.Duration.Inf
import concurrent.Await

class CanLoadSimpleProject extends SbtClientTest {
  // TODO - Don't hardcode sbt versions, unless we have to...
  val dummy = utils.makeDummySbtProject("test")

  withSbt(dummy) { client =>
    val build = concurrent.promise[MinimalBuildStructure]
    import concurrent.ExecutionContext.Implicits.global
    client watchBuild build.success
    val result = Await.result(build.future, defaultTimeout)
    assert(result.projects.size == 1, "Found too many projects!")
    val project = result.projects.head
    assert(project.name == "test", "failed to discover project name == file name.")

    // Here we check autocompletions:
    val completes = Await.result(client.possibleAutocompletions("hel", 0), defaultTimeout)
    assert(completes.exists(_.append == "p"), "Failed to autocomplete `help` command.")
  }

}