package com.typesafe.sbtrc
package it
package execution

import sbt.client._
import sbt.protocol._
import concurrent.duration.Duration.Inf
import concurrent.{ Await, ExecutionContext, Promise }
import collection.JavaConversions
import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import scala.annotation.tailrec
import java.util.concurrent.Executors
import play.api.libs.json.Reads

class TestExecution extends SbtClientTest {

  val executorService = Executors.newSingleThreadExecutor()
  implicit val keepEventsInOrderExecutor = ExecutionContext.fromExecutorService(executorService)

  try {
    final case class ExecutionRecord(results: Map[ScopedKey, TaskResult], events: Seq[Event])

    def recordExecutions(client: SbtClient, commands: Seq[String]): concurrent.Future[Map[String, ExecutionRecord]] = {
      val results = new LinkedBlockingQueue[(ScopedKey, TaskResult)]()
      val events =
        commands.foldLeft(Map.empty[String, LinkedBlockingQueue[Event]]) { (sofar, next) =>
          sofar + (next -> new LinkedBlockingQueue[Event]())
        }
      val executionDones =
        commands.foldLeft(Map.empty[String, concurrent.Promise[Unit]]) { (sofar, next) =>
          sofar + (next -> concurrent.Promise[Unit]())
        }
      @volatile var commandsById = Map.empty[Long, String]
      val futureWatches = for {
        dep1 <- client.lookupScopedKey("dep1").map(_.head)
        dep2 <- client.lookupScopedKey("dep2").map(_.head)
        end1 <- client.lookupScopedKey("end1").map(_.head)
        end2 <- client.lookupScopedKey("end2").map(_.head)
        trigger1 <- client.lookupScopedKey("trigger1").map(_.head)
        waitForStampFile <- client.lookupScopedKey("waitForStampFile").map(_.head)
      } yield {
        def saveResult: RawValueListener = { (key, result) =>
          results.add(key -> result)
        }
        Seq(client.rawLazyWatch(TaskKey[Int](dep1))(saveResult),
          client.rawLazyWatch(TaskKey[String](dep2))(saveResult),
          client.rawLazyWatch(TaskKey[Int](end1))(saveResult),
          client.rawLazyWatch(TaskKey[String](end2))(saveResult),
          client.rawLazyWatch(TaskKey[Int](trigger1))(saveResult),
          client.rawLazyWatch(TaskKey[Int](waitForStampFile))(saveResult))
      }
      @volatile var executionsByTask = Map(0L -> 0L)
      def handleEvent(event: Event): Unit = {
        def record(id: Long): Unit = {
          if (id == 0L) {
            // untargeted events get recorded for all commands (not ideal really,
            // but then we shouldn't have untargeted events...)
            for (queue <- events.values)
              queue.add(event)
          } else {
            commandsById.get(id) foreach { command =>
              events.get(command) foreach { _.add(event) }
            }
          }
        }
        event match {
          case ExecutionWaiting(id, command, clientInfo) =>
            commandsById += (id -> command)
            record(id)
          case ExecutionStarting(id) =>
            record(id)
          case ExecutionFailure(id) =>
            if (!commandsById.contains(id))
              throw new RuntimeException(s"ExecutionFailure on unknown id: ${event}")
            record(id)
            executionDones.get(commandsById(id)).foreach(_.success(()))
          case ExecutionSuccess(id) =>
            if (!commandsById.contains(id))
              throw new RuntimeException(s"ExecutionSuccess on unknown id: ${event}")
            record(id)
            executionDones.get(commandsById(id)).foreach(_.success(()))
          case TaskStarted(id, taskId, _) =>
            record(id)
            executionsByTask += (taskId -> id)
          case TaskFinished(id, taskId, _, _) =>
            record(id)
            executionsByTask -= taskId
          case TaskLogEvent(taskId, _) =>
            executionsByTask.get(taskId)
              .map(record(_))
              .getOrElse(System.err.println(s"log event from unknown task $taskId ${event}"))
          case other =>
            System.out.println(s"Not recording event: ${other}")
        }
      }
      val eventHandler = client.handleEvents(handleEvent)
      val futureTestDone = for {
        watches <- futureWatches
        // we ignore the result of requestExecution and instead use
        // ExecutionWaiting because ExecutionWaiting is supposed to
        // be guaranteed to happen before ExecutionFailure/ExecutionSuccess
        _ <- concurrent.Future.sequence(commands map { command =>
          client.requestExecution(command, interaction = None)
        })
        _ <- concurrent.Future.sequence(executionDones.values.map(_.future)) // wait for complete
      } yield {
        var resultsMap = Map.empty[ScopedKey, TaskResult]
        while (!results.isEmpty())
          resultsMap += results.take()

        commands.foldLeft(Map.empty[String, ExecutionRecord]) { (sofar, next) =>
          var eventsList = List.empty[Event]
          val ourEvents = events.get(next).getOrElse(throw new Exception("No events for " + next))
          while (!ourEvents.isEmpty())
            eventsList ::= ourEvents.take()
          val record = ExecutionRecord(resultsMap, eventsList.reverse)
          sofar + (next -> record)
        }
      }
      futureTestDone.onComplete { _ =>
        futureWatches.map(_.map(_.cancel()))
        eventHandler.cancel()
      }
      futureTestDone
    }

    def recordExecution(client: SbtClient, command: String): concurrent.Future[ExecutionRecord] = {
      recordExecutions(client, Seq(command)) map { _.values.head }
    }

    // sequence must match expected items in order, but may have other items too
    def verifySequence(outermost: Seq[_], expecteds: Seq[PartialFunction[Any, Unit]]): Unit = {
      System.err.println(s"Verifying ${expecteds.size} items from ${outermost.size}")
      def verifyOne(items: List[_], expected: PartialFunction[Any, Unit]): List[_] = items match {
        case head :: tail =>
          if (expected.isDefinedAt(head)) {
            expected.apply(head)
            // uncomment this to find failures
            // System.err.println("Matched: " + head)
            tail
          } else {
            verifyOne(tail, expected)
          }
        case Nil =>
          // not that PartialFunction.toString is useful... uncomment
          // above println which logs each match to see where we stop
          throw new AssertionError(s"No items matching ${expected} in ${outermost}")
      }

      @tailrec
      def verifyList(items: List[_], expecteds: Seq[PartialFunction[Any, Unit]]): Unit = {
        expecteds.toList match {
          case head :: tail =>
            val remaining = verifyOne(items, head)
            verifyList(remaining, tail)
          case Nil =>
        }
      }

      verifyList(outermost.toList, expecteds)
    }

    def checkSuccess[T](record: ExecutionRecord, taskName: String, expected: T)(implicit reads: Reads[T]): Unit = {
      record.results.collect({
        case (key, result) if key.key.name == taskName =>
          result
      }).headOption match {
        case Some(TaskSuccess(value)) if (value.value[T] == Some(expected)) => // ok!
        case Some(TaskSuccess(value)) =>
          throw new AssertionError(s"Value of ${taskName} was was ${value}, expected Some(${expected})")
        case wrong =>
          throw new AssertionError(s"Result of ${taskName} was was ${wrong}, expected Some(TaskSuccess(${expected}))")
      }
    }

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
       |
       | val waitForStampFile = taskKey[Unit]("Wait for target/stamp.txt to exist")
       |
       | waitForStampFile := {
       |   while (!file("target/stamp.txt").exists)
       |     Thread.sleep(100)
       | }
       |
       |""".stripMargin)

    System.out.println(s"Launching IT ${this.getClass.getName}")

    val successfulTestCount = new java.util.concurrent.atomic.AtomicInteger(0)
    withSbt(dummy) { client =>
      val build = Promise[MinimalBuildStructure]

      client watchBuild build.trySuccess
      val result = waitWithError(build.future, "Never got build structure.")
      assert(result.projects.size == 1, "Found too many projects!")
      val project = result.projects.head
      assert(project.id.name == "execution", "failed to discover project name == file name.")
      assert(project.plugins contains "sbt.plugins.JvmPlugin", s"failed to discover default plugins in project, found: ${project.plugins.mkString(", ")}")

      // TEST 1: Try a single task with no dependencies
      {
        System.out.println("Testing single task with no dependencies")
        val recordedDep1 = waitWithError(recordExecution(client, "dep1"), "didn't get dep1 result")

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
            case TaskLogEvent(taskId, LogStdOut(message)) if message == "dep1-stdout" =>
              assert(taskId == dep1TaskId)
          },
          {
            case TaskLogEvent(taskId, LogStdErr(message)) if message == "dep1-stderr" =>
              assert(taskId == dep1TaskId)
          },
          {
            case TaskLogEvent(taskId, LogMessage(LogMessage.INFO, message)) if message == "dep1-info" =>
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

        successfulTestCount.getAndIncrement()
      }

      // TEST 2: Task 'end1' with deps 'dep1' and 'dep2' which triggers 'trigger1'
      {
        System.out.println("Testing triggered tasks")
        val recordedEnd1 = waitWithError(recordExecution(client, "end1"), "didn't get end1 result")

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
        successfulTestCount.getAndIncrement()
      }

      // TEST 3: Task ';end1;end2' (multiple end-nodes in the task graph)
      {
        System.out.println("Testing multiple end nodes in the task graph")
        val recordedEnd1End2 = waitWithError(recordExecution(client, ";end1;end2"), "didn't get ;end1;end2 result")

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

        successfulTestCount.getAndIncrement()
      }

      // TEST 4: Test that a second client can connect and see the execution queue
      // Note: this test is last in a withSbt because the nested withSbt sends a
      // requestSelfDestruct to the server which would confuse following tests
      // without some work to clean up the problem.
      {
        System.out.println("Testing that a second client gets the execution queue")
        // Block the execution queue with a task that waits for target/stamp.txt to exist,
        // then put two more things in the queue so we can see the order
        val futureFirstClientRecord = recordExecutions(client, Seq("waitForStampFile", "dep1", "dep2"))

        // now make a second client, after a pause to allow the waitForStampFile to
        // become active... bad hack alert
        Thread.sleep(1000)
        withSbt(dummy) { client2 =>
          val events = new LinkedBlockingQueue[Event]()
          // set up to watch events... we should get a replay of
          // the execution queue.
          def handleEvent(event: Event): Unit = {
            events.add(event)
          }
          val eventHandler = client2.handleEvents(handleEvent)

          // create stamp file so executions run and everything starts to play out
          sbt.IO.write(new java.io.File(dummy, "target/stamp.txt"), "Hi!")

          def complete(status: Map[String, List[Event]]): Boolean = {
            status.values.foldLeft(true) { (completed, next) =>
              completed && next.exists {
                case _: ExecutionFailure | _: ExecutionSuccess => true
                case _ => false
              }
            }
          }

          def nextEvent(): Event =
            Option(events.poll(defaultTimeout.toMillis,
              java.util.concurrent.TimeUnit.MILLISECONDS))
              .getOrElse(throw new Exception("Timed out waiting for event on second client"))

          def processNext(event: Event, sofar: Map[String, List[Event]]): Map[String, List[Event]] = {
            def findCommand(byId: Long): String = {
              val allCommands = sofar.values.collect {
                case (ExecutionWaiting(id, command, _) :: tail) if id == byId =>
                  command
              }
              allCommands.headOption.getOrElse {
                throw new Exception(s"No such ID ${byId}")
              }
            }
            def append(id: Long): Map[String, List[Event]] = {
              val command = findCommand(id)
              val old = sofar.get(command).getOrElse(throw new Exception(s"unexpected command ${command}"))
              sofar + (command -> (old :+ event))
            }
            val recorded = event match {
              case ExecutionWaiting(id, command, clientInfo) =>
                if (sofar.get(command).map(_.nonEmpty).getOrElse(false))
                  throw new Exception(s"Got something else before ExecutionWaiting for ${command} already had: ${sofar.get(command)} just got: ${event}")
                sofar + (command -> (event :: Nil))
              case ExecutionStarting(id) => append(id)
              case ExecutionFailure(id) => append(id)
              case ExecutionSuccess(id) => append(id)
              case _ => sofar
            }
            if (complete(recorded))
              recorded
            else
              processNext(nextEvent(), recorded)
          }

          // waitForStampFile isn't in the list because currently new clients
          // are not sent the *active* execution only the waiting ones.
          val allCommands = List( /* "waitForStampFile", */ "dep1", "dep2")
          val startingEvents = Map(allCommands.map(c => c -> List.empty[Event]): _*)
          val secondClientEvents = processNext(nextEvent(), startingEvents)
          for (command <- allCommands) {
            var executionId = -1L
            secondClientEvents.get(command) map { events =>
              verifySequence(events,
                Seq(
                  {
                    case ExecutionWaiting(id, commandFound, clientInfo) if command == commandFound =>
                      executionId = id
                      // Note: these should be from "client" not "client2"!
                      assert(clientInfo.uuid == client.uuid.toString)
                      assert(clientInfo.configName == client.configName)
                      assert(clientInfo.humanReadableName == client.humanReadableName)
                  },
                  {
                    case ExecutionStarting(id) =>
                      assert(id == executionId)
                  },
                  {
                    case ExecutionSuccess(id) =>
                      assert(id == executionId)
                  }))
            } getOrElse {
              throw new Exception(s"No events for $command in ${secondClientEvents}")
            }
          }
        }
        val recorded = waitWithError(futureFirstClientRecord, "first client didn't get the events")
        def verifyExecution(name: String): Unit = {
          var executionId = -1L
          verifySequence(recorded(name).events, Seq(
            {
              case ExecutionWaiting(id, command, clientInfo) if ((command: String) == name) =>
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
              case TaskStarted(id, taskId, Some(key)) if key.key.name == name =>
                assert(id == executionId)
            },
            {
              case TaskFinished(id, taskId, Some(key), success) if key.key.name == name =>
                assert(id == executionId)
                assert(success)
            },
            {
              case ExecutionSuccess(id) =>
                assert(id == executionId)
            }))
        }
        verifyExecution("waitForStampFile")
        verifyExecution("dep1")
        verifyExecution("dep2")

        successfulTestCount.getAndIncrement()
      }
    }

    // test 5 needs a new client since it wants to close the connection early
    withSbt(dummy) { client =>
      val build = Promise[MinimalBuildStructure]

      client watchBuild build.trySuccess
      val result = waitWithError(build.future, "Never got build structure.")
      assert(result.projects.size == 1, "Found too many projects!")
      val project = result.projects.head
      assert(project.id.name == "execution", "failed to discover project name == file name.")
      assert(project.plugins contains "sbt.plugins.JvmPlugin", s"failed to discover default plugins in project, found: ${project.plugins.mkString(", ")}")

      // TEST 5: Test that outstanding tasks and executions get proper finished events on close
      {
        System.out.println("Testing that we get finished events on close")

        // delete any leftover stamp file
        (new java.io.File(dummy, "target/stamp.txt")).delete()

        val pendingWaitForStampFile = Promise[Unit]()
        client.handleEvents {
          case TaskStarted(_, _, Some(key)) if key.key.name == "waitForStampFile" =>
            pendingWaitForStampFile.trySuccess(())
          case other =>
            System.err.println("While waiting for stamp: " + other)
        }

        // Block the execution queue with a task that waits for target/stamp.txt to exist,
        // then put two more things in the queue so we can see the order
        val futureFirstClientRecord = recordExecutions(client, Seq("waitForStampFile", "dep1", "dep2"))

        waitWithError(pendingWaitForStampFile.future, "didn't get ExecutionWaiting(waitForStampFile)")

        // kill the connection with stuff still pending
        // NOTE this leaves an sbt server stuck in waitForStampFile that will never exit,
        // so we have to clean up below
        client.close()

        val recorded = waitWithError(futureFirstClientRecord, "client didn't get the events")
        def verifyExecutionFailed(name: String): Unit = {
          var executionId = -1L
          verifySequence(recorded(name).events, Seq(
            {
              case ExecutionWaiting(id, command, clientInfo) if ((command: String) == name) =>
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
              case TaskStarted(id, taskId, Some(key)) if key.key.name == name =>
                assert(id == executionId)
            },
            {
              case TaskFinished(id, taskId, Some(key), success) if key.key.name == name =>
                assert(id == executionId)
                assert(!success)
            },
            {
              case ExecutionFailure(id) =>
                assert(id == executionId)
            }))
        }
        def verifyExecutionNeverRun(name: String): Unit = {
          var executionId = -1L
          verifySequence(recorded(name).events, Seq(
            {
              case ExecutionWaiting(id, command, clientInfo) if ((command: String) == name) =>
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
              case ExecutionFailure(id) =>
                assert(id == executionId)
            }))
        }

        // BEFORE we run the tests that may fail, unstick the
        // server so we don't get a bunch of leftover processes
        sbt.IO.write(new java.io.File(dummy, "target/stamp.txt"), "Hi!")

        verifyExecutionFailed("waitForStampFile")
        verifyExecutionNeverRun("dep1")
        verifyExecutionNeverRun("dep2")

        successfulTestCount.getAndIncrement()
      }
    }

    if (successfulTestCount.get == 5)
      System.out.println(s"Done with IT ${this.getClass.getName}")
    else
      throw new AssertionError("Did not complete all tests successfully")

  } finally {
    executorService.shutdown()
  }
}
