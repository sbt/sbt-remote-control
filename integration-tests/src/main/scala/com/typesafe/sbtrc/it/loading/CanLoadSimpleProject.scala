package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import concurrent.duration.Duration.Inf
import concurrent.Await
import java.io.File
import sbt.client.ScopedKey

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
    assert(project.id.name == "test", "failed to discover project name == file name.")
    assert(project.plugins contains "sbt.plugins.JvmPlugin", s"failed to discover default plugins in project, found: ${project.plugins.mkString(", ")}")

    // Here we check autocompletions:
    val completes = waitWithError(client.possibleAutocompletions("hel", 0), "Autocompletions not returned in time.")
    assert(completes.exists(_.append == "p"), "Failed to autocomplete `help` command.")

    def testTask(requestExecution: => concurrent.Future[Long]): Unit = {
      // Attempt to call a custom task and ensure we get our stdout events.
      val stdoutCaptured = concurrent.promise[Unit]
      val logInfoCaptured = concurrent.promise[Unit]
      val stderrCaptured = concurrent.promise[Unit]
      val stdoutSub = (client handleEvents {
        case LogEvent(taskId, LogStdOut(line)) if line contains "test-out" =>
          if (taskId != 0)
            stdoutCaptured.success(())
          else
            stdoutCaptured.failure(new RuntimeException("task ID was 0 for task stdout"))
        case LogEvent(taskId, LogStdErr(line)) if line contains "test-err" =>
          if (taskId != 0)
            stderrCaptured.success(())
          else
            stderrCaptured.failure(new RuntimeException("task ID was 0 for task stderr"))
        case LogEvent(taskId, LogMessage("info", line)) if line contains "test-info" =>
          if (taskId != 0)
            logInfoCaptured.success(())
          else
            logInfoCaptured.failure(new RuntimeException("task ID was 0 for task log info"))
        case _ =>
      })(global)
      requestExecution
      // Now we wait for the futures to fill out.
      waitWithError(stdoutCaptured.future, "Unable to read known stdout lines from server")
      waitWithError(stderrCaptured.future, "Unable to read known stderr lines from server")
      waitWithError(logInfoCaptured.future, "Unable to read known log info lines from server")
      stdoutSub.cancel()
    }
    testTask(client.requestExecution("printOut", None))
    testTask {
      client.lookupScopedKey("printOut") flatMap { keys => client.requestExecution(keys.head, None) }
    }

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

    val keysFuture = client.lookupScopedKey("compile")
    val keys = waitWithError(keysFuture, "Never received key lookup response!")
    assert(!keys.isEmpty && keys.head.key.name == "compile", s"Failed to find compile key: $keys!")

    // check receiving the value of a setting key
    val baseDirectoryKeysFuture = client.lookupScopedKey(s"${project.id.name}/baseDirectory")
    val baseDirectoryKeys = waitWithError(baseDirectoryKeysFuture, "Never received key lookup response!")

    val baseDirectoryPromise = concurrent.Promise[File]
    client.watch(SettingKey[File](baseDirectoryKeys.head)) { (a, b) =>
      b match {
        case TaskSuccess(file) =>
          baseDirectoryPromise.trySuccess(file.value.get)
        case TaskFailure(msg) =>
          baseDirectoryPromise.tryFailure(new Exception(msg))
      }
    }

    val baseDirectory = waitWithError(baseDirectoryPromise.future, "Never received watch setting key first value")
    assert(dummy.getAbsoluteFile() == baseDirectory, s"Failed to received correct baseDirectory: $baseDirectory")

    // check receiving the initial value of a task key
    val unmanagedSourcesKeysFuture = client.lookupScopedKey(s"${project.id.name}/compile:unmanagedSources")
    val unmanagedSourcesKeys = waitWithError(unmanagedSourcesKeysFuture, "Never received key lookup response!")

    val unmanagedSourcesPromise = concurrent.Promise[collection.Seq[File]]
    client.watch(TaskKey[collection.Seq[File]](unmanagedSourcesKeys.head)) { (a, b) =>
      b match {
        case TaskSuccess(files) =>
          unmanagedSourcesPromise.trySuccess(files.value.get)
        case TaskFailure(msg) =>
          unmanagedSourcesPromise.tryFailure(new Exception(msg))
      }
    }

    val unmanagedSources = waitWithError(unmanagedSourcesPromise.future, "Never received watch task key first value")
    val expectedSources =
      Seq(
        new File(dummy, "src/main/scala/hello.scala").getCanonicalFile,
        new File(dummy, "src/main/scala/error.scala").getCanonicalFile)
    assert(unmanagedSources.sorted == expectedSources.sorted, s"Failed to received correct unmanagedSources: $unmanagedSources, expected $expectedSources")

  }

}