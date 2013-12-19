package com.typesafe.sbtrc

import com.typesafe.sbtrc.protocol._
import com.typesafe.sbtrc.it._
import java.io.File
import akka.actor._
import akka.pattern._
import akka.dispatch._
import concurrent.duration._
import concurrent.Await
import akka.util.Timeout
import sbt.IO
import java.util.concurrent.TimeoutException
import com.typesafe.sbtrc.protocol.RequestReceivedEvent

abstract class CanRunEchoProject(val sbtVersionString: String, val taskName: String, val taskParams: Map[String, Any]) extends SbtProcessLauncherTest {

  val dummy = utils.makeEmptySbtProject("runEcho22", sbtVersionString)
  val plugins = new File(dummy, "project/plugins.sbt")
  // TODO - Pull the echo version from properties...
  if (!(sbtVersionString startsWith "0.13")) {
    IO.write(plugins,
      """addSbtPlugin("com.typesafe.sbt" % "sbt-echo" % "0.1.0")""")
  }
  val build = new File(dummy, "build.sbt")
  IO.write(build,
    (if (sbtVersionString startsWith "0.13") ""
    else "echoSettings") +
      """

name := "test-app"

scalaVersion := "2.10.2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.0"
""")
  val appSource = new File(dummy, "src/main/scala/Main.scala")
  IO.write(appSource,
    """
      object Main {
         def main(args: Array[String]): Unit = {
           akka.actor.ActorSystem("test").shutdown
         }
      }
  """)
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  @volatile var receivedNameInfo = false
  try {
    val result = concurrent.promise[Response]()
    val testActor = system.actorOf(Props(new Actor with ActorLogging {
      var askedToStop = false
      context.setReceiveTimeout(120.seconds)

      val request = GenericRequest(sendEvents = true, taskName, taskParams)
      // Let's issue two requests, one for name and one for other.
      child ! NameRequest(sendEvents = true)
      child ! request
      log.debug("Sent run request " + request)

      def receive: Receive = {
        // Here we capture the result of the Name task
        case x: NameResponse =>
          log.debug("Received name response " + x)
          receivedNameInfo =
            (x.attributes.getOrElse("hasAkka", false).asInstanceOf[Boolean] &&
              !x.attributes.getOrElse("hasPlay", false).asInstanceOf[Boolean] &&
              x.attributes.getOrElse("hasEcho", false).asInstanceOf[Boolean])
        // Here we capture the result of the run task.
        case x: RunResponse =>
          log.debug("Received run response " + x)
          result.success(x)
          context stop self

        case ReceiveTimeout =>
          // If we haven't received any events in a while, here's what we do.
          result.failure(new RuntimeException("Nothing has happened in a long time, giving up"))
          context stop self
        case log: LogEvent =>
        // ignore log event
        case e: Event =>
          log.debug("Got an event from the run request: " + e)
      }
    }), "can-run-sbt-13-and-echo")

    Await.result(result.future, timeout.duration) match {
      case RunResponse(success, name) if name == taskName =>
        if (!receivedNameInfo)
          throw new AssertionError("Did not discover echo/akka support via name request!")
      case whatever =>
        throw new AssertionError("did not get RunResponse for " + taskName + " got " + whatever)
    }
  } finally {
    system.stop(child)
  }
}

/** Ensures that we can make requests and receive responses from our children. */
class CanRunSbt13EchoProject extends CanRunEchoProject(TestUtil.sbt13TestVersion, TaskNames.runEcho, Map("tracePort" -> 32461))

class CanRunSbt12EchoProject extends CanRunEchoProject(TestUtil.sbt12TestVersion, TaskNames.runEcho, Map("tracePort" -> 32462))

class CanRunMainSbt13EchoProject extends CanRunEchoProject(TestUtil.sbt13TestVersion, TaskNames.runMainEcho, Map("mainClass" -> "Main", "tracePort" -> 32463))

