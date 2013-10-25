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

/** Ensures that we can get a default main when there are multiple. */
abstract class CanGetDefaultMain(val sbtVersion: String, val forceMain2: Boolean) extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProjectWithMultipleMain("getDefaultMain_" + forceMain2 + "_" + sbtVersion, sbtVersion = sbtVersion)
  if (forceMain2) {
    val f = new File(dummy, "main2.sbt")
    utils.createFile(f, "mainClass in Compile := Some(\"Main2\")\n")
  }

  class DieIfWeSeeMainSelector extends Actor {
    val child = SbtProcess(context, dummy, sbtProcessLauncher)
    var replyTo: Option[ActorRef] = None
    override def receive = {
      case "go" =>
        replyTo = Some(sender)
        child ! MainClassRequest(sendEvents = true)
      case e: LogEvent if e.entry.message.contains("[1] Main1") =>
        System.err.println("Log event looks like sbt is asking user for the main class: " + e)
        val exc = new RuntimeException("NOPE user was asked for the main class " + e)
        replyTo foreach { _ ! Status.Failure(exc) }
        throw exc
      case r: Response =>
        replyTo foreach { _ ! r }
      case whatever =>
      // ignore
    }
  }
  val a = system.actorOf(Props(new DieIfWeSeeMainSelector), name = "die-on-see-main-selector")
  try {
    Await.result(a ? "go", timeout.duration) match {
      case MainClassResponse(Some(mainName)) =>
        val expected = if (forceMain2) "Main2" else "Main1"
        if (mainName != expected)
          throw new AssertionError("got wrong main class: '" + mainName + "'")
        else
          println("Got expected main class: " + expected)
      case whatever => throw new AssertionError("unexpected result sending MainClassRequest to app with multi main method: " + whatever)
    }
  } finally {
    system.stop(a)
  }
}

class CanGetDefaultMainSbt12 extends CanGetDefaultMain("0.12.4", forceMain2 = false)
class CanGetDefaultMainSbt13 extends CanGetDefaultMain("0.13.0", forceMain2 = false)
class CanGetForcedDefaultMainSbt12 extends CanGetDefaultMain("0.12.4", forceMain2 = true)
class CanGetForcedDefaultMainSbt13 extends CanGetDefaultMain("0.13.0", forceMain2 = true)
