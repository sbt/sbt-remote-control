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

abstract class CanTraceAkkaProjects(val akkaVersion: String, val sbtVersionString: String, val shouldAddEcho: Boolean) extends SbtProcessLauncherTest {

  val projectName = "trace-with-akka-" + akkaVersion.replaceAllLiterally(".", "-")
  val dummy = utils.makeEmptySbtProject(projectName, sbtVersionString)
  val build = new File(dummy, "build.sbt")
  IO.write(build,
    s"""
name := "test-app"

scalaVersion := "2.10.2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "${akkaVersion}"
""")
  val appSource = new File(dummy, "src/main/scala/Main.scala")
  IO.write(appSource,
    """
      object Main {
         def main(args: Array[String]): Unit = {
           // long enough for atmos to start up
           Thread.sleep(30*1000L)
         }
      }
  """)
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  @volatile var receivedSocketInfo = false
  @volatile var receivedNameInfo = false
  try {
    val result = concurrent.promise[Unit]()
    val testActor = system.actorOf(Props(new Actor with ActorLogging {
      var askedToStop = false
      context.setReceiveTimeout(120.seconds)

      child ! NameRequest(sendEvents = true)

      def receive: Receive = {
        // Here we capture the result of the Name task
        case x: NameResponse =>
          log.debug("Received name response " + x)
          receivedNameInfo = true
          val hasAkka = x.attributes.getOrElse("hasAkka", false).asInstanceOf[Boolean]
          val hasConsole = x.attributes.getOrElse("hasEcho", false).asInstanceOf[Boolean]

          if (hasAkka && shouldAddEcho == hasConsole) {
            result.success(())
          } else {
            if (hasAkka)
              result.failure(new Exception(s"shouldAddEcho=${shouldAddEcho} for akka=${akkaVersion} but detected hasConsole=${hasConsole}"))
            else
              result.failure(new Exception("Failed to detect akka in test project that should have had it"))
          }
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
    }), "can-trace")

    Await.result(result.future, timeout.duration)
  } catch {
    case t: TimeoutException if (!receivedSocketInfo) =>
      sys.error("Failed to analyze project before timing out!")
  } finally {
    system.stop(child)
  }
}

class CanTraceAkka22 extends CanTraceAkkaProjects(akkaVersion = BuildInfo.supportedAkkaVersionSbt013, sbtVersionString = TestUtil.sbt13TestVersion, shouldAddEcho = true)
class CannotTraceAkka23 extends CanTraceAkkaProjects(akkaVersion = "2.3.0-RC2", sbtVersionString = TestUtil.sbt13TestVersion, shouldAddEcho = false)
