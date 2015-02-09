package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import sbt.serialization._
import concurrent.duration.Duration.Inf
import concurrent.Await
import concurrent.Promise
import scala.collection.JavaConverters._
import scala.util.Success

final case class MyEvent(value: Int)
object MyEvent extends DetachedEventUnapply[MyEvent] {
  implicit val pickler = PicklerUnpickler.generate[MyEvent]
}

// Tests using sbt-core-next plugin... TODO rename this file/class
class CanUseUiInteractionPlugin extends SbtClientTest {
  // TODO - Don't hardcode sbt versions, unless we have to...
  val dummy = utils.makeDummySbtProject("interactions")

  // TODO - Add sbt-ui-plugin correctly...

  // TODO - Core next version comes from build.
  sbt.IO.write(new java.io.File(dummy, "project/ui.sbt"),
    s"""addSbtPlugin("org.scala-sbt" % "sbt-core-next" % "0.1.1")""")

  sbt.IO.write(new java.io.File(dummy, "project/custom.scala"),
    """|package com.typesafe.sbtrc
       |package it
       |package loading
       |import sbt.plugins.InteractionServicePlugin._
       |import sbt.plugins.SerializersPlugin._
       |import sbt.serialization._
       |import sbt._
       |
       |final case class SerializedThing(name: String, value: Int)
       |object SerializedThing {
       |  import sbt.serialization._
       |  implicit val pickler: Pickler[SerializedThing] = genPickler[SerializedThing]
       |  implicit val unpickler: Unpickler[SerializedThing] = genUnpickler[SerializedThing]
       |}
       |
       |final case class MyEvent(value: Int)
       |object MyEvent {
       |  implicit val pickler = PicklerUnpickler.generate[MyEvent]
       |}
       |
       |object TestThingPlugin {
       |   import sbt.serialization._
       |   val makeTestThing = taskKey[SerializedThing]("makes a test thing")
       |
       |   val sendDetachedEvent = taskKey[Unit]("Send a detached event")
       |
       |   def settings: Seq[Setting[_]] =
       |     Seq(
       |       registerTaskSerialization(makeTestThing),
       |       makeTestThing := SerializedThing("Hello, it's a test!", 10),
       |       sendDetachedEvent := {
       |          sbt.SendEventService.getDetached(Keys.state.value).foreach(_.sendEvent(MyEvent(42)))
       |       }
       |     )
       |}
       |""".stripMargin)

  // TODO - create custom type and register it.
  sbt.IO.write(new java.io.File(dummy, "interaction.sbt"),
    """|import sbt.InteractionServiceKeys.interactionService
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

  withSbt(dummy) { client =>
    import concurrent.ExecutionContext.global

    // test detached events
    val detachedCaptured = Promise[Unit]
    (client handleEvents {
      case MyEvent(e) if e.value == 42 =>
        detachedCaptured.success(())
      case e: DetachedEvent =>
        System.err.println(s"unexpected DetachedEvent received: $e")
      case other =>
    })(global)

    client.requestExecution("sendDetachedEvent", None)

    waitWithError(detachedCaptured.future, "Unable to read known detached event from server")

    // test asking for input
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
      case TaskFinished(executionId, taskId, key, result, messageOption) =>
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
    val testThingValuePromise = Promise[sbt.protocol.TaskResult]
    client.lookupScopedKey("makeTestThing").foreach {
      case Seq(key) =>
        client.rawWatch(TaskKey[SerializedThing](key)) { (key: ScopedKey, value: TaskResult) =>
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
