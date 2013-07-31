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

/** Ensures that we can make requests and receive responses from our children. */
class CanHandleBrokenBuild extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProjectWithBrokenBuild("brokenBuild")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  try {
    Await.result(child ? RunRequest(sendEvents = false, mainClass = None), timeout.duration) match {
      case ErrorResponse(message) if message.contains("sbt process never got in touch") =>
      case whatever => throw new AssertionError("unexpected result sending RunRequest to broken build: " + whatever)
    }
  } finally {
    system.stop(child)
  }
}