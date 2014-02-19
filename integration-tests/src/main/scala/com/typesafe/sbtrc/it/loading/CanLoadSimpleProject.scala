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

  sbt.IO.write(new java.io.File(dummy, "stdout.sbt"),

    """| val printOut = taskKey[Unit]("Print to stdout and see if we get it.")
       |  
       | printOut := System.out.println("test-out")
       |""".stripMargin)

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

    // Attempt to call a custom task and ensure we get our stdout events.
    val stdoutCaptured = concurrent.promise[Unit]
    val stdoutSub = (client handleEvents {
      case LogEvent(LogStdOut(line)) if line contains "test-out" =>
        stdoutCaptured.success(())
      case _ =>
    })(global)
    client.requestExecution("printOut", None)
    Await.result(stdoutCaptured.future, defaultTimeout)
  }

}