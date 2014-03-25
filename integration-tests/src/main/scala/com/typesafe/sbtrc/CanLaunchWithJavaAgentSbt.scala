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
abstract class CanLaunchWithJavaAgent(val sbtTestVersion: String) extends SbtProcessLauncherTest {
  val dummy = utils.makeEmptySbtProject("runWithAgent", sbtTestVersion)
  val build = new File(dummy, "build.sbt")
  val agentJar = "./../../../../../../example-agent/target/sbt-rc-example-agent-1.0.jar"
  IO.write(build, s"""
import java.io.File

name := "test-app"

scalaVersion := "2.10.3"

externalDependencyClasspath in Compile += new File("$agentJar")

unmanagedClasspath in Runtime += new File("$agentJar")
""")
  val appSource = new File(dummy, "src/main/scala/Main.scala")
  IO.write(appSource,
    """
     import com.typesafe.sbtrc.agent.ExampleAgent

      object Main {
         def main(args: Array[String]): Unit = {
          println(s"agent string: ${ExampleAgent.getAgentString()}")
         }
      }
  """)
  val child = SbtProcess(system, dummy, sbtProcessLauncher, extraJvmArgs = Seq(s"-javaagent:$agentJar"))
  try {
    val result = concurrent.promise[String]()
    val testActor = system.actorOf(Props(new Actor with ActorLogging {
      var askedToStop = false
      context.setReceiveTimeout(120.seconds)

      val request = RunRequest(sendEvents = false, mainClass = None)
      child ! SubscribeOutput(self)
      child ! request
      log.debug("Sent run request " + request)

      def receive: Receive = {
        // Here we capture the result of the Name task
        case x: RunResponse =>
        // ignore

        case ReceiveTimeout =>
          // If we haven't received any events in a while, here's what we do.
          result.failure(new RuntimeException("Nothing has happened in a long time, giving up"))
          context stop self
        case ErrorResponse(message) =>
          result.failure(new RuntimeException(message))
          context stop self
        case LogEvent(LogStdOut("agent string: running")) =>
          result.success("running")
          child ! UnsubscribeOutput(self)
          context stop self
        case LogEvent(LogStdOut("agent string: null")) =>
          result.success("null")
          child ! UnsubscribeOutput(self)
          context stop self
        case l: LogEvent =>
        // ignore
        case e: Event =>
          log.debug("Got an event from the run request: " + e)
      }
    }), s"can-launch-with-java-agent-$sbtTestVersion")

    Await.result(result.future, timeout.duration) match {
      case "running" => // success!
      case "null" =>
        throw new AssertionError("Java agent not installed.  Expected result 'running' got 'null'")
    }
  } catch {
    case t: TimeoutException =>
      sys.error("Unable to get result from instrumented process")
  } finally {
    system.stop(child)
  }
}

class CanLaunchWithJavaAgentSbt13 extends CanLaunchWithJavaAgent(TestUtil.sbt13TestVersion)

class CanLaunchWithJavaAgentSbt12 extends CanLaunchWithJavaAgent(TestUtil.sbt12TestVersion)
