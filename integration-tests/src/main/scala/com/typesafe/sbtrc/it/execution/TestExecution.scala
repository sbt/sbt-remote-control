package com.typesafe.sbtrc
package it
package execution

import sbt.client._
import sbt.protocol._
import concurrent.duration.Duration.Inf
import concurrent.Await
import collection.JavaConversions
import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import scala.annotation.tailrec
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

class TestExecution extends SbtClientTest {
  val dummy = utils.makeDummySbtProject("execution")

  sbt.IO.write(new java.io.File(dummy, "execution.sbt"),

    """| val dep1 = taskKey[Int]("A dependency 1.")
       |
       | dep1 := {
       |   System.out.println("dep1-stdout")
       |   System.err.println("dep1-stderr")
       |   streams.value.log.info("dep1-info")
       |   1
       | }
       |
       | val dep2 = taskKey[String]("A dependency 2.")
       |
       | dep2 := {
       |   System.out.println("dep2-stdout")
       |   System.err.println("dep2-stderr")
       |   streams.value.log.info("dep2-info")
       |   // an anonymous task with no key
       |   (Def.task { 103 }).value
       |   "2"
       | }
       |
       | val end1 = taskKey[Long]("An endpoint task 1")
       |
       | end1 := {
       |   streams.value.log.info("end1-info")
       |   dep1.value + Integer.parseInt(dep2.value)
       | }
       |
       | val end2 = taskKey[String]("An endpoint task 2")
       |
       | end2 := {
       |   streams.value.log.info("end2-info")
       |   dep1.value.toString + dep2.value
       | }
       |
       | val trigger1 = taskKey[Int]("A task triggered by end1")
       |
       | trigger1 := {
       |   streams.value.log.info("trigger1-info")
       |   dep1.value
       | }
       |
       | // TODO is there a better way to write this?
       | trigger1 <<= trigger1 triggeredBy end1
       |""".stripMargin)

  withSbt(dummy) { client =>
    val executorService = Executors.newSingleThreadExecutor()
    implicit val keepEventsInOrderExecutor = ExecutionContext.fromExecutorService(executorService)
    try {
      val build = concurrent.promise[MinimalBuildStructure]

      client watchBuild build.trySuccess
      val result = waitWithError(build.future, "Never got build structure.")
      assert(result.projects.size == 1, "Found too many projects!")
      val project = result.projects.head
      assert(project.id.name == "execution", "failed to discover project name == file name.")
      assert(project.plugins contains "sbt.plugins.JvmPlugin", s"failed to discover default plugins in project, found: ${project.plugins.mkString(", ")}")

      case class ExecutionRecord(results: Map[ScopedKey, sbt.client.TaskResult[_]], events: Seq[Event])

      def recordExecution(command: String): concurrent.Future[ExecutionRecord] = {
        val results = new LinkedBlockingQueue[(ScopedKey, sbt.client.TaskResult[_])]()
        val events = new LinkedBlockingQueue[Event]()
        val executionDone = concurrent.Promise[Unit]()
        val futureWatches = for {
          dep1 <- client.lookupScopedKey("dep1").map(_.head)
          dep2 <- client.lookupScopedKey("dep2").map(_.head)
          end1 <- client.lookupScopedKey("end1").map(_.head)
          end2 <- client.lookupScopedKey("end2").map(_.head)
          trigger1 <- client.lookupScopedKey("trigger1").map(_.head)
        } yield {
          def saveResult[T]: ValueListener[T] = { (key, result) =>
            results.add(key -> result)
          }
          Seq(client.lazyWatch(TaskKey[Int](dep1))(saveResult),
            client.lazyWatch(TaskKey[String](dep2))(saveResult),
            client.lazyWatch(TaskKey[Int](end1))(saveResult),
            client.lazyWatch(TaskKey[String](end2))(saveResult),
            client.lazyWatch(TaskKey[Int](trigger1))(saveResult))
        }
        def handleEvent(event: Event): Unit = {
          events.add(event)
          event match {
            case _: ExecutionFailure | _: ExecutionSuccess =>
              executionDone.success()
            case _ =>
          }
        }
        val eventHandler = client.handleEvents(handleEvent)
        val futureTestDone = for {
          watches <- futureWatches
          executionId <- client.requestExecution(command, interaction = None)
          _ <- executionDone.future // wait for complete
        } yield {
          var resultsMap = Map.empty[ScopedKey, sbt.client.TaskResult[_]]
          while (!results.isEmpty())
            resultsMap += results.take()
          var eventsList = List.empty[Event]
          while (!events.isEmpty())
            eventsList ::= events.take()
          ExecutionRecord(resultsMap, eventsList.reverse)
        }
        futureTestDone.onComplete { _ =>
          futureWatches.map(_.map(_.cancel()))
          eventHandler.cancel()
        }
        futureTestDone
      }

      // sequence must match expected items in order, but may have other items too
      @tailrec
      def verifySequence(items: Seq[_], expecteds: Seq[PartialFunction[Any, Unit]]): Unit = {
        def verifyOne(items: List[_], expected: PartialFunction[Any, Unit]): List[_] = items match {
          case head :: tail =>
            if (expected.isDefinedAt(head)) {
              expected.apply(head)
              // uncomment this to find failures
              //System.err.println("Matched: " + head)
              tail
            } else {
              verifyOne(tail, expected)
            }
          case Nil =>
            // not that PartialFunction.toString is useful... uncomment
            // above println which logs each match to see where we stop
            throw new AssertionError(s"No items matching ${expected}")
        }

        expecteds.toList match {
          case head :: tail =>
            val remaining = verifyOne(items.toList, head)
            verifySequence(remaining, tail)
          case Nil =>
        }
      }

      def checkSuccess[T](record: ExecutionRecord, taskName: String, expected: T): Unit = {
        record.results.collect({
          case (key, result) if key.key.name == taskName =>
            result
        }).headOption match {
          case Some(TaskSuccess(value)) if ((value.value: Option[_]) == Some(expected)) => // ok!
          case Some(TaskSuccess(value)) =>
            throw new AssertionError(s"Value of ${taskName} was was ${value}, expected Some(${expected})")
          case wrong =>
            throw new AssertionError(s"Result of ${taskName} was was ${wrong}, expected Some(TaskSuccess(${expected}))")
        }
      }

      // TEST 1: Try a single task with no dependencies
      {
        val recordedDep1 = waitWithError(recordExecution("dep1"), "didn't get dep1 result")

        checkSuccess(recordedDep1, "dep1", 1)

        var executionId = -1L
        var dep1TaskId = -1L
        verifySequence(recordedDep1.events, Seq(
          {
            case ExecutionWaiting(id, command, clientInfo) if ((command: String) == "dep1") =>
              executionId = id
              assert(clientInfo.uuid == client.uuid.toString)
              assert(clientInfo.configName == client.configName)
              assert(clientInfo.humanReadableName == client.humanReadableName)
          },
          {
            case ExecutionStarting(id) =>
              assert(id == executionId)
          },
          {
            case TaskStarted(id, taskId, Some(key)) if key.key.name == "dep1" =>
              assert(id == executionId)
              dep1TaskId = taskId
          },
          {
            case LogEvent(taskId, LogStdOut(message)) if message == "dep1-stdout" =>
              assert(taskId == dep1TaskId)
          },
          {
            case LogEvent(taskId, LogStdErr(message)) if message == "dep1-stderr" =>
              assert(taskId == dep1TaskId)
          },
          {
            case LogEvent(taskId, LogMessage(LogMessage.INFO, message)) if message == "dep1-info" =>
              assert(taskId == dep1TaskId)
          },
          {
            case TaskFinished(id, taskId, Some(key), success) if key.key.name == "dep1" =>
              assert(id == executionId)
              assert(taskId == dep1TaskId)
              assert(success)
          },
          {
            case ExecutionSuccess(id) =>
              assert(id == executionId)
          }))
      }

      // TEST 2: Task 'end1' with deps 'dep1' and 'dep2' which triggers 'trigger1'
      {
        val recordedEnd1 = waitWithError(recordExecution("end1"), "didn't get end1 result")

        checkSuccess(recordedEnd1, "dep1", 1)
        checkSuccess(recordedEnd1, "dep2", "2")
        checkSuccess(recordedEnd1, "end1", 3)
        checkSuccess(recordedEnd1, "trigger1", 1)

        var executionId = -1L
        var end1TaskId = -1L
        verifySequence(recordedEnd1.events, Seq(
          {
            case ExecutionWaiting(id, command, clientInfo) if ((command: String) == "end1") =>
              executionId = id
              assert(clientInfo.uuid == client.uuid.toString)
              assert(clientInfo.configName == client.configName)
              assert(clientInfo.humanReadableName == client.humanReadableName)
          },
          {
            case ExecutionStarting(id) =>
              assert(id == executionId)
          },
          {
            case TaskStarted(id, _, Some(key)) if key.key.name == "dep1" =>
              assert(id == executionId)
          },
          {
            case TaskStarted(id, taskId, Some(key)) if key.key.name == "end1" =>
              assert(id == executionId)
              end1TaskId = taskId
          },
          {
            case TaskFinished(id, taskId, Some(key), success) if taskId == end1TaskId && key.key.name == "end1" =>
              assert(id == executionId)
              assert(taskId == end1TaskId)
              assert(success)
          },
          {
            case TaskStarted(id, _, Some(key)) if key.key.name == "trigger1" =>
              assert(id == executionId)
          },
          {
            case TaskFinished(id, _, Some(key), success) if key.key.name == "trigger1" =>
              assert(id == executionId)
          },
          {
            case ExecutionSuccess(id) =>
              assert(id == executionId)
          }))
      }

      // TEST 3: Task ';end1;end2' (multiple end-nodes in the task graph)
      {
        val recordedEnd1End2 = waitWithError(recordExecution(";end1;end2"), "didn't get ;end1;end2 result")

        // the main thing to check here is that both end1 and end2 are going to run
        checkSuccess(recordedEnd1End2, "dep1", 1)
        checkSuccess(recordedEnd1End2, "dep2", "2")
        checkSuccess(recordedEnd1End2, "end1", 3)
        checkSuccess(recordedEnd1End2, "end2", "12")
        checkSuccess(recordedEnd1End2, "trigger1", 1)

        var executionId = -1L
        var end1TaskId = -1L
        verifySequence(recordedEnd1End2.events, Seq(
          {
            case ExecutionWaiting(id, command, clientInfo) if ((command: String) == ";end1;end2") =>
              executionId = id
              assert(clientInfo.uuid == client.uuid.toString)
              assert(clientInfo.configName == client.configName)
              assert(clientInfo.humanReadableName == client.humanReadableName)
          },
          {
            case ExecutionStarting(id) =>
              assert(id == executionId)
          },
          {
            case TaskStarted(id, _, Some(key)) if key.key.name == "dep1" =>
              assert(id == executionId)
          },
          {
            case TaskStarted(id, taskId, Some(key)) if key.key.name == "end1" =>
              assert(id == executionId)
              end1TaskId = taskId
          },
          {
            case TaskFinished(id, taskId, Some(key), success) if taskId == end1TaskId && key.key.name == "end1" =>
              assert(id == executionId)
              assert(taskId == end1TaskId)
              assert(success)
          },
          {
            case ExecutionSuccess(id) =>
              assert(id == executionId)
          }))
      }

    } finally {
      executorService.shutdown()
    }
  }
}
