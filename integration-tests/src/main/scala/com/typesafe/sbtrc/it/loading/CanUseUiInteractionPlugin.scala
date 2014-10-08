package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import concurrent.duration.Duration.Inf
import concurrent.Await
import concurrent.Promise
import scala.collection.JavaConverters._
import play.api.libs.json.Json
import scala.util.Success

// Tests using interaction...
class CanUseUiInteractionPlugin extends SbtClientTest {
  // TODO - Don't hardcode sbt versions, unless we have to...
  val dummy = utils.makeDummySbtProject("interactions")

  // TODO - Add sbt-ui-plugin correctly...

  sbt.IO.write(new java.io.File(dummy, "project/ui.sbt"),
    s"""addSbtPlugin("com.typesafe.sbtrc" % "ui-interface-0-13" % "${sys.props("project.version")}")""")

  sbt.IO.write(new java.io.File(dummy, "project/custom.scala"),
    """|package com.typesafe.sbtrc
       |package it
       |package loading
       |import play.api.libs.json._
       |import sbt.SbtUIPlugin._
       |import sbt._
       | 
       |final case class SerializedThing(name: String, value: Int)
       |object SerializedThing {
       |  implicit val format = Json.format[SerializedThing]
       |}
       |
       |
       |object TestThingPlugin {
       |   val makeTestThing = taskKey[SerializedThing]("makes a test thing")
       |   def settings: Seq[Setting[_]] =
       |     Seq(
       |       registerTaskSerialization(makeTestThing),
       |       makeTestThing := SerializedThing("Hello, it's a test!", 10)
       |     )
       |}
       |""".stripMargin)

  // TODO - create custom type and register it.
  sbt.IO.write(new java.io.File(dummy, "interaction.sbt"),
    """|import sbt.UIKeys.interactionService
       |import com.typesafe.sbtrc.it.loading.TestThingPlugin
       |
       | val readInput = taskKey[Unit]("Quick interaction with server test.")
       |  
       | readInput := {
       |   val contex = (interactionService in Global).?.value
       |   contex match {
       |     case Some(ctx) =>
       |       if(!ctx.confirm("test-confirm")) sys.error("COULD NOT CONFIRM TEST!")
       |       val line = ctx.readLine("> ", false)
       |       if(line != Some("test-line")) sys.error("COULD NOT READ LINE! - Got " + line)
       |       ()
       |     // This happens if server isn't loaded.
       |     case None => sys.error("NO INTERACTION SERVICE DEFINED!")
       |   }
       | }
       |
       |TestThingPlugin.settings
       |""".stripMargin)

  withSbt(dummy, sbt.protocol.DynamicSerialization.defaultSerializations.register(SerializedThing.format)) { client =>
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
    val taskResult = Promise[Boolean]
    val executionResult = Promise[Boolean]
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

    // Now we try to grab the value of maketestThing 
    // To do this we had to create the client with serializer SerializedThing.format
    val testThingValuePromise = Promise[sbt.protocol.TaskResult[Any, Throwable]]
    client.lookupScopedKey("makeTestThing").foreach {
      case Seq(key) =>
        client.rawWatch(TaskKey[SerializedThing](key)) { (key: ScopedKey, value: TaskResult[Any, Throwable]) =>
          testThingValuePromise.trySuccess(value)
        }(global)
    }(global)
    val testThingValue = Await.result(testThingValuePromise.future, defaultTimeout)
    assert(testThingValue.isSuccess, "Failed to run makeTestThing")
    val sbt.protocol.TaskSuccess(buildValue) = testThingValue
    testThingValue.result[SerializedThing] match {
      case Success(value) =>
        assert(value.name == "Hello, it's a test!", "Failed to serialize custom name attribute")
        assert(value.value == 10, "Failed to serialize custom value attribute")
      case _ => sys.error(s"Failed to serialize SerializedThing: ${testThingValue}")
    }
  }

}