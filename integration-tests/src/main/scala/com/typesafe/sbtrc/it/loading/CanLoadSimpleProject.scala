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
       | printOut := {
       |   System.out.println("test-out")
       |   System.err.println("test-err")
       |   streams.value.log.info("test-info")
       | }
       |""".stripMargin)

  sbt.IO.write(new java.io.File(dummy, "src/main/scala/error.scala"),

    """object Foo(x: String)""".stripMargin)

  withSbt(dummy) { client =>
    val build = concurrent.promise[MinimalBuildStructure]
    import concurrent.ExecutionContext.Implicits.global
    client watchBuild build.success
    val result = waitWithError(build.future, "Never got build structure.")
    assert(result.projects.size == 1, "Found too many projects!")
    val project = result.projects.head
    assert(project.name == "test", "failed to discover project name == file name.")

    // Here we check autocompletions:
    val completes = waitWithError(client.possibleAutocompletions("hel", 0), "Autocompletions not returned in time.")
    assert(completes.exists(_.append == "p"), "Failed to autocomplete `help` command.")

    // Attempt to call a custom task and ensure we get our stdout events.
    val stdoutCaptured = concurrent.promise[Unit]
    val logInfoCaptured = concurrent.promise[Unit]
    val stderrCaptured = concurrent.promise[Unit]
    val stdoutSub = (client handleEvents {
      case LogEvent(LogStdOut(line)) if line contains "test-out" =>
        stdoutCaptured.success(())
      case LogEvent(LogStdErr(line)) if line contains "test-err" =>
        stderrCaptured.success(())
      case LogEvent(LogMessage("info", line)) if line contains "test-info" =>
        logInfoCaptured.success(())
      case _ =>
    })(global)
    client.requestExecution("printOut", None)
    // Now we wait for the futures to fill out.
    waitWithError(stdoutCaptured.future, "Unable to read known stdout lines from server")
    waitWithError(stderrCaptured.future, "Unable to read known stderr lines from server")
    waitWithError(logInfoCaptured.future, "Unable to read known log info lines from server")
    stdoutSub.cancel()

    // Now we check compilation failure messages
    val compileErrorCaptured = concurrent.promise[CompilationFailure]
    val compileErrorSub = (client handleEvents {
      case x: CompilationFailure =>
        compileErrorCaptured.success(x)
      case _ =>
    })(global)
    client.requestExecution("compile", None)
    val error = waitWithError(compileErrorCaptured.future, "Never received compilation failure!")
    assert(error.severity == xsbti.Severity.Error, "Failed to capture appropriate error.")
  }

}