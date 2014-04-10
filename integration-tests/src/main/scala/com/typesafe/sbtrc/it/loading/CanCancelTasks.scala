package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import java.util.concurrent.Executors
import concurrent.duration.Duration.Inf
import concurrent.{ Await, ExecutionContext }
import java.io.File
import sbt.client.ScopedKey
import java.util.concurrent.LinkedBlockingQueue
import annotation.tailrec

class CanCancelTasks extends SbtClientTest {
  // TODO - Don't hardcode sbt versions, unless we have to...
  val dummy = utils.makeDummySbtProject("test")

  sbt.IO.write(new java.io.File(dummy, "looper.sbt"),

    """| val infiniteLoop = taskKey[Unit]("Runs forever to test cancelling.")
       |  
       | infiniteLoop := {
       |   while(true) { Thread.sleep(10L) }
       | }
       |""".stripMargin)

  withSbt(dummy) { client =>
    val executorService = Executors.newSingleThreadExecutor()
    implicit val keepEventsInOrderExecutor = ExecutionContext.fromExecutorService(executorService)
    //import concurrent.ExecutionContext.Implicits.global
    case class ExecutionRecord(loopCancelled: Boolean, compileCancelled: Boolean, events: Seq[Event])

    def recordExecution(): concurrent.Future[ExecutionRecord] = {
      val results = new LinkedBlockingQueue[(ScopedKey, sbt.client.TaskResult[_])]()
      val events = new LinkedBlockingQueue[Event]()
      var loopIdValue = 0L
      val allDone = concurrent.promise[Unit]
      def handleEvent(event: Event): Unit = {
        event match {
          case ExecutionWaiting(id, "infiniteLoop") => loopIdValue = id
          case ExecutionFailure(id) => if (id == loopIdValue) allDone.success(())
          case _ =>
        }
        events.add(event)
      }
      val eventHandler = client.handleEvents(handleEvent)
      val futureTestDone = for {
        loopId <- client.requestExecution("infiniteLoop", None)
        compileId <- client.requestExecution("compile", None)
        compile <- client.cancelExecution(compileId)
        loop <- client.cancelExecution(loopId)
        _ <- allDone.future
      } yield {
        var eventsList = List.empty[Event]
        while (!events.isEmpty()) {
          eventsList ::= events.take()
        }
        ExecutionRecord(loop, compile, eventsList.reverse)
      }
      futureTestDone.onComplete { _ =>
        eventHandler.cancel()
      }
      futureTestDone
    }

    def await[T](f: concurrent.Future[T]): T = Await.result(f, defaultTimeout)
    val ExecutionRecord(l, c, events) = await(recordExecution())
    //assert(l, "Failed to cancel infinite loop task!")
    //assert(c, "Failed to cancel a work item in the queue.")
    // Check the ordering of events int he sequence.
    // sequence must match expected items in order, but may have other items too
    @tailrec
    def verifySequence(items: Seq[_], expecteds: Seq[PartialFunction[Any, Unit]]): Unit = {
      def verifyOne(nextItems: List[_], expected: PartialFunction[Any, Unit]): List[_] = nextItems match {
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
          System.err.println("-- Sequence did not match expected --")
          items map (" * ".+) foreach System.err.println
          throw new AssertionError(s"No items matching ${expected}")
      }

      expecteds.toList match {
        case head :: tail =>
          val remaining = verifyOne(items.toList, head)
          verifySequence(remaining, tail)
        case Nil =>
      }
    }
    var loopId: Long = 0L
    var compileId: Long = 0L
    case class NamedPf[T, U](name: String, pf: PartialFunction[T, U]) extends PartialFunction[T, U] {
      def isDefinedAt(t: T): Boolean = pf.isDefinedAt(t)
      def apply(t: T): U = pf(t)
      override def toString = name
    }
    verifySequence(events,
      Seq(
        NamedPf("infiniteLoopWaiting", {
          case ExecutionWaiting(id, command) if ((command: String) == "infiniteLoop") => loopId = id
        }),
        NamedPf("infiniteLoopStarted", {
          case ExecutionStarting(id) => assert(id == loopId)
        }),
        // Note: This generally happens before the loop starts because the build is booting.
        NamedPf("compileWaiting", {
          case ExecutionWaiting(id, command) if ((command: String) == "compile") => compileId = id
        }),

        NamedPf("compileStarted", {
          case ExecutionStarting(id) => assert(id == compileId)
        }),
        NamedPf("compileFailed", { case ExecutionFailure(id) => assert(id == compileId) }),

        // Here we canceled the main task.
        NamedPf("loopFailed", { case ExecutionFailure(id) => assert(id == loopId) })))

  }

}