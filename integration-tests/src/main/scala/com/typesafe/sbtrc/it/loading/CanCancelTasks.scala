package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import concurrent.duration.Duration.Inf
import concurrent.Await
import java.io.File
import sbt.client.ScopedKey

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
    import concurrent.ExecutionContext.Implicits.global
    val infiniteLoopId = concurrent.promise[Long]
    val executionResult = concurrent.promise[Boolean]
    (client handleEvents {
      case ExecutionSuccess(id) =>
        (infiniteLoopId.future map { iid =>
          if (id == iid) executionResult.success(true)
        })(global)
      case ExecutionFailure(id) =>
        (infiniteLoopId.future map { iid =>
          if (id == iid) executionResult.success(false)
        })(global)
      case _ =>
    })(global)
    val idOfForever = client.requestExecution("infiniteLoop", None)
    (idOfForever onSuccess {
      case id =>
        infiniteLoopId success id
    })(global)

    // Here we use the infinite loop to test out cancelling tasks in the queue.
    val cancelInQueue = (client.requestExecution("compile", None) flatMap { id =>
      client.cancelExecution(id)
    })(global)

    // TODO - ensure no logs about the compile task ever complete.

    // Here we cancel the infinite loop
    val resultOfCancel = (idOfForever flatMap { id =>
      client.cancelExecution(id)
    })(global)
    def await[T](f: concurrent.Future[T]): T = Await.result(f, defaultTimeout)
    assert(await(resultOfCancel), "Failed to cancel infinit task!")
    assert(!await(executionResult.future), "Cancelled execution should be unsuccessful!")
    assert(await(cancelInQueue), "Failed to cancel a work item in the queue.")
  }

}