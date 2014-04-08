package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._

import concurrent.duration.Duration.Inf
import concurrent.Await

import scala.collection.JavaConverters._

// Tests using interaction...
class CanUseUiInteractionPlugin extends SbtClientTest {
  // TODO - Don't hardcode sbt versions, unless we have to...
  val dummy = utils.makeDummySbtProject("interactions")

  // TODO - Add sbt-ui-plugin correctly...

  sbt.IO.write(new java.io.File(dummy, "interaction.sbt"),

    """|import sbt.UIContext.uiContext
       |
       | val readInput = taskKey[Unit]("Quick interaction with server test.")
       |  
       | readInput := {
       |   val contex = (uiContext in Global).?.value
       |   contex match {
       |     case Some(ctx) =>
       |       if(!ctx.confirm("test-confirm")) sys.error("COULD NOT CONFIRM TEST!")
       |       val line = ctx.readLine("> ", false)
       |       if(line != Some("test-line")) sys.error("COULD NOT READ LINE! - Got " + line)
       |       ()
       |     // This happens if server isn't loaded.
       |     case None => sys.error("NO UI CONTEXT DEFINED!")
       |   }
       | }
       |""".stripMargin)

  withSbt(dummy) { client =>
    // Here we request something to run which will ask for input...
    import concurrent.ExecutionContext.global
    object interaction extends Interaction {
      def readLine(prompt: String, mask: Boolean): Option[String] = Some("test-line")
      def confirm(msg: String): Boolean = {
        if (msg == "test-confirm") true
        else false
      }
    }
    val events = new java.util.concurrent.ConcurrentLinkedQueue[Event]
    val taskResult = concurrent.promise[Boolean]
    val executionResult = concurrent.promise[Boolean]
    (client handleEvents {
      case TaskFinished(executionId, taskId, key, result) =>
        if (key.map(_.key.name) == Some("readInput")) {
          taskResult.success(result)
        }
      case ExecutionSuccess(id) =>
        executionResult.success(true)
      case ExecutionFailure(id) =>
        executionResult.success(false)
      case other => events.add(other)
    })(global)
    // Here we want to wait for the task to be done.
    client.requestExecution("readInput", Some(interaction -> global))
    assert(Await.result(taskResult.future, defaultTimeout), "Failed to interact with sbt task!")
    assert(Await.result(executionResult.future, defaultTimeout), "Failed to get ExecutionSuccess")
    val eventSet = events.iterator().asScala.toSet
    assert(eventSet.collect({ case e: ExecutionWaiting => e }).nonEmpty, s"Execution was never queued up, got ${eventSet}")
    assert(eventSet.collect({ case e: ExecutionStarting => e }).nonEmpty, s"Execution was never started, got ${eventSet}")
  }

}