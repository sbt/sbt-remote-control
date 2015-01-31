package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import sbt.serialization._
import java.io.File
import java.util.concurrent.Executors
import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{ Await, ExecutionContext, Promise, Future }
import scala.util.{ Success, Failure }

// This test just has some things we want to test with Java specifically,
// generic tests are in CanLoadSimpleProject
class CanLoadJavaProject extends SbtClientTest {
  // it's important that this project contains no scala files,
  // so we use javac and test its error reporting.
  val dummy = utils.makeEmptySbtProject("testjava")

  val sbtFile = new java.io.File(dummy, "build.sbt")

  sbt.IO.write(sbtFile,
    """name := "testjava" """.stripMargin)

  val errorFile = new java.io.File(dummy, "src/main/java/Fail.java")
  errorFile.getParentFile.mkdirs()

  sbt.IO.write(errorFile,
    """
      blahblahblah;
      """.stripMargin)

  // if you uncomment this, then we use scalac and the test
  // passes.
  // val errorScalaFile = new java.io.File(dummy, "src/main/scala/Fail.scala")
  // errorScalaFile.getParentFile.mkdirs()
  //sbt.IO.write(errorScalaFile,
  //  """
  //    foofoofoo;
  //    """.stripMargin)

  withSbt(dummy) { client =>
    val build = Promise[MinimalBuildStructure]
    val executorService = Executors.newSingleThreadExecutor()
    implicit val keepEventsInOrderExecutor = ExecutionContext.fromExecutorService(executorService)
    client watchBuild build.trySuccess
    val result = waitWithError(build.future, "Never got build structure.")
    assert(result.projects.size == 1, "Found too many projects!")
    val project = result.projects.head
    assert(project.id.name == "testjava", "failed to discover project name == file name.")
    assert(project.plugins contains "sbt.plugins.JvmPlugin", s"failed to discover default plugins in project, found: ${project.plugins.mkString(", ")}")

    val compileKeysFuture = client.lookupScopedKey("compile")
    val compileKeys = waitWithError(compileKeysFuture, "Never received key lookup response!")
    assert(!compileKeys.isEmpty && compileKeys.head.key.name == "compile", s"Failed to find compile key: $compileKeys!")

    // Now we check compilation failure messages

    var compileId = 0L
    val compileErrorCaptured = Promise[CompilationFailure]
    val compileErrorSub = (client handleEvents {
      case CompilationFailure(taskId, failure) =>
        System.out.println(s"Capturing CompilationFailure $failure")
        compileErrorCaptured.trySuccess(failure)
      case ExecutionWaiting(id, command, _) if command.indexOf("compile") >= 0 =>
        compileId = id
      case ExecutionFailure(id) if id == compileId =>
        compileErrorCaptured.tryFailure(new AssertionError(s"compile execution $compileId failed with no CompilationFailure"))
      case ExecutionSuccess(id) if id == compileId =>
        compileErrorCaptured.tryFailure(new AssertionError(s"compile execution $compileId succeeded but we wanted a CompilationFailure"))
      case _ =>
    })(keepEventsInOrderExecutor)

    def withCompileTaskResult(body: Future[Unit] => Unit): Unit = {
      val result = Promise[Unit]
      val compileWatchSub: Subscription = (client.rawWatch(TaskKey[Unit](compileKeys.head)) { (a: ScopedKey, b: TaskResult) =>
        result.tryComplete(b.resultWithCustomThrowable[Unit, CompileFailedException])
      })(keepEventsInOrderExecutor)

      try body(result.future)
      finally compileWatchSub.cancel()
    }
    withCompileTaskResult { compileWatchFuture =>
      client.requestExecution("compile", None)
      val gotException =
        try {
          waitWithError(compileWatchFuture, "Unable get compile analysis from server")
          false
        } catch {
          case e: CompileFailedException =>
            System.out.println(s"Got CompileFailedException $e")
            if (e.problems.isEmpty)
              throw new AssertionError(s"CompileFailedException had no problems in it $e")
            else if (!e.problems.head.position.lineContent.contains("blahblahblah"))
              throw new AssertionError(s"CompileFailedException had unexpected lineContent $e")
            else
              System.out.println(s"Got expected CompileFailedException message=${e.getMessage} problems=${e.problems}")
            true
          case e: Throwable =>
            throw new AssertionError(s"expected CompileFailedException, got $e")
            true
        }
      if (!gotException)
        throw new AssertionError(s"Expected compile to fail but it didn't")
    }

    val error = try {
      waitWithError(compileErrorCaptured.future, "Never received compilation failure!")
    } finally {
      compileErrorSub.cancel()
    }
    System.out.println(s"Got expected CompilationFailure $error")
    assert(error ne null, "compilation failure event was null")
    assert(error.severity == xsbti.Severity.Error, "Failed to capture appropriate error.")
    assert(error.position.lineContent.contains("blahblahblah"), "CompilationFailure contained appropriate lineContent")
  }
}
