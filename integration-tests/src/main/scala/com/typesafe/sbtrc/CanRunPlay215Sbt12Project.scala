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
class CanRunPlay215Sbt12Project extends SbtProcessLauncherTest {
  val dummy = utils.makeEmptySbtProject("runPlay215", TestUtil.sbt12TestVersion)
  val plugins = new File(dummy, "project/plugins.sbt")
  // TODO - Abstract out plugin version to test more than one play instance...
  IO.write(plugins,
    """addSbtPlugin("play" % "sbt-plugin" % "2.1.5")""")
  val build = new File(dummy, "project/build.scala")
  IO.write(build,
    """
import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName = "integrationtest"
  val appVersion = "0.2"

  val appDependencies = Seq()

  val main = 
      play.Project(appName, appVersion, appDependencies).
      settings(
        resolvers += Resolver.url("typesafe-ivy-releases", new URL("http://private-repo.typesafe.com/typesafe/releases"))(Resolver.ivyStylePatterns),
        resolvers += ("typesafe-mvn-releases" at "http://private-repo.typesafe.com/typesafe/releases")
      )
}
""")
  val appconf = new File(dummy, "conf/application.conf")
  IO.write(appconf,
    """
    """)
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  @volatile var receivedSocketInfo = false
  @volatile var receivedNameInfo = false
  try {
    val result = concurrent.promise[Response]()
    val testActor = system.actorOf(Props(new Actor with ActorLogging {
      var askedToStop = false
      context.setReceiveTimeout(120.seconds)

      child ! NameRequest(sendEvents = true)
      def receive: Receive = {
        case x: NameResponse =>
          log.debug("Received name response " + x)
          receivedNameInfo =
            x.projects.head.attributes.getOrElse("hasPlay", false).asInstanceOf[Boolean]
          // Now issue the run request
          child ! RunRequest(sendEvents = true, mainClass = None)
        // Here we capture the result of the run task.
        case x: RunResponse =>
          result.success(x)
          context stop self

        // Here we capture the output of play start. 
        // TODO - We should validate the port is the one we expect....
        case GenericEvent("playServerStarted", params) =>
          receivedSocketInfo = true
          // Now we can manually cancel
          self ! ReceiveTimeout

        // If we haven't received any events in a while, here's what we do.
        case ReceiveTimeout =>
          // First we ask ourselves to stop and wait for the result to come
          // back.  If that takes too long, we explode IN YOUR FACE!
          if (!askedToStop) {
            child ! CancelRequest
            context.setReceiveTimeout(30.seconds)
            askedToStop = true
          } else {
            // For Play 2.1.x, we don't care if we can cancel or not...
            result.failure(new TimeoutException("Failed to cancel task within timeout!."))
            context stop self
          }
        case _: Event =>
        // Ignore all other events, but let them block our receive timeouts...
      }
    }), "can-run-sbt-12-and-play")
    Await.result(result.future, timeout.duration) match {
      case RunResponse(success, "run") =>
        println("DEBUGME: RunResponse = " + success)
        if (!receivedSocketInfo)
          throw new AssertionError("did not receive a play socket we can listen on!")
        if (!receivedNameInfo)
          throw new AssertionError("Did not discover atmos/akka support via name request!")
      case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
    }
  } catch {
    case t: TimeoutException if receivedSocketInfo =>
    // Ignore.  It's ok to not be able to cancel for now, since activator just kills the entire
    // sbt instance when it needs to cancel things.
    case t: TimeoutException =>
      sys.error("Failed to start play server before timing out!")
  } finally {
    system.stop(child)
  }
}
