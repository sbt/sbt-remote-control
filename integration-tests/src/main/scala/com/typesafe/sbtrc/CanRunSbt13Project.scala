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
class CanRunSbt13Project extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProject("runChild13", TestUtil.sbt13TestVersion)
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  try {
    Await.result(child ? RunRequest(sendEvents = false, mainClass = None), timeout.duration) match {
      case RunResponse(success, "run") =>
      case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
    }
  } finally {
    system.stop(child)
  }
}
