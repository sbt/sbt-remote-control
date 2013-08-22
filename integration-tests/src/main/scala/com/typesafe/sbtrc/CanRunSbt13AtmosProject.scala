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

/** Ensures that we can make requests and receive responses from our children. */
class CanRunSbt13AtmosProject extends SbtProcessLauncherTest {
  val dummy = utils.makeEmptySbtProject("runAtmos22", "0.13.0-RC5")
  val plugins = new File(dummy, "project/plugins.sbt")
  IO.write(plugins,
    """addSbtPlugin("com.typesafe.sbt" % "sbt-atmos" % "0.2.2")""")
  val build = new File(dummy, "build.sbt")
  IO.write(build,
    """atmosSettings
      
name := "test-app"
""")
  val appSource = new File(dummy, "src/main/scala/Main.scala")
  IO.write(appSource,
    """
      object Main {
         def main(args: Array[String]): Unit = {
           Thread.sleep(60*1000L)
         }
      }
  """)
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  // TODO - Check to see if atmos has properly started up...
  try {
    val result = concurrent.promise[Response]()
    val testActor = system.actorOf(Props(new Actor with ActorLogging {
      context.setReceiveTimeout(2.minutes)
      def receive: Receive = {
        // Here we capture the result of the run task.
        case x: RunResponse =>
          result.success(x)
          context stop self

        // If we haven't received any events in a while, here's what we do.
        case ReceiveTimeout =>
          // First we ask ourselves to stop and wait for the result to come
          // back.  If that takes too long, we explode IN YOUR FACE!
          result.failure(new RuntimeException("Failed to cancel task within timeout!."))
          context stop self
        case _: Event =>
        // Ignore all other events, but let them block our receive timeouts...
      }
    }), "can-run-sbt-13-and-atmos")

    val request =
      GenericRequest(sendEvents = true, TaskNames.runAtmos, Map.empty)
    child.tell(RunRequest(sendEvents = true, mainClass = None), testActor)
    Await.result(result.future, timeout.duration) match {
      case RunResponse(true, "run") => ()
      case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
    }
  } finally {
    system.stop(child)
  }
}
