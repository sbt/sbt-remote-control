package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import java.util.concurrent.Executors
import concurrent.duration.Duration.Inf
import concurrent.{ Await, ExecutionContext, Promise }
import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import annotation.tailrec

class CanCancelTasks extends SbtClientTest {
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
    final case class ExecutionRecord(loopCancelled: Boolean, compileCancelled: Boolean, events: Seq[Event])

    def recordExecution(): concurrent.Future[ExecutionRecord] = {
      val results = new LinkedBlockingQueue[(ScopedKey, TaskResult[_, Throwable])]()
      val events = new LinkedBlockingQueue[Event]()
      var executionsEndedCount = 0
      val allDone = Promise[Unit]
      def handleEvent(event: Event): Unit = {
        event match {
          case ExecutionFailure(id) => executionsEndedCount += 1
          case ExecutionSuccess(id) => executionsEndedCount += 1
          case _ =>
        }
        events.add(event)
        // note that executionsEndedCount will be 2 for multiple
        // events (the one where we incremented it, plus any others
        // after that)
        if (executionsEndedCount == 2)
          allDone.trySuccess(())
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
    assert(l, "Failed to cancel infinite loop task!")
    assert(c, "Failed to cancel a work item in the queue.")
    // Check the ordering of events int he sequence.
    // sequence must match expected items in order, but may have other items too
    @tailrec
    def verifySequence(items: Seq[_], expecteds: Seq[PartialFunction[Any, Unit]]): Unit = {
      def verifyOne(nextItems: List[_], expected: PartialFunction[Any, Unit]): List[_] = nextItems match {
        case head :: tail =>
          if (expected.isDefinedAt(head)) {
            expected.apply(head)
            tail
          } else {
            verifyOne(tail, expected)
          }
        case Nil =>
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
    final case class NamedPf[T, U](name: String, pf: PartialFunction[T, U]) extends PartialFunction[T, U] {
      def isDefinedAt(t: T): Boolean = pf.isDefinedAt(t)
      def apply(t: T): U = pf(t)
      override def toString = name
    }
    // we verify the two sequences separately since their order of
    // interleaving is not guaranteed, i.e. we might get both waiting, then
    // one starting, or one waiting, then starting, then the other waiting,
    // or whatever.

    verifySequence(events,
      Seq(
        NamedPf("infiniteLoopWaiting", {
          case ExecutionWaiting(id, command, client) if ((command: String) == "infiniteLoop") => loopId = id
        }),
        NamedPf("infiniteLoopStarted", {
          case ExecutionStarting(id) if id == loopId =>
        }),
        NamedPf("loopFailed", { case ExecutionFailure(id) if id == loopId => })))

    verifySequence(events,
      Seq(
        NamedPf("compileWaiting", {
          case ExecutionWaiting(id, command, client) if ((command: String) == "compile") => compileId = id
        }),
        NamedPf("compileStarted", {
          case ExecutionStarting(id) if id == compileId =>
        }),
        NamedPf("compileFailed", { case ExecutionFailure(id) if id == compileId => })))

    // We do check ordering between specific event pairs, though.
    // The two executions have to wait and start in the order we
    // queue them.
    verifySequence(events,
      Seq(
        NamedPf("infiniteLoopWaitingFirst", {
          case ExecutionWaiting(id, command, client) if ((command: String) == "infiniteLoop") =>
        }),
        NamedPf("compileWaitingSecond", {
          case ExecutionWaiting(id, command, client) if ((command: String) == "compile") =>
        })))

    verifySequence(events,
      Seq(
        NamedPf("infiniteLoopStartedFirst", {
          case ExecutionStarting(id) if id == loopId =>
        }),
        NamedPf("compileStartedSecond", {
          case ExecutionStarting(id) if id == compileId =>
        })))

    // the order of ExecutionFailure events is NOT guaranteed, because we may
    // fail the compile execution in the process of canceling the infiniteLoop
    // execution (compile fails with RejectedExecutionException in that case)
  }
}
