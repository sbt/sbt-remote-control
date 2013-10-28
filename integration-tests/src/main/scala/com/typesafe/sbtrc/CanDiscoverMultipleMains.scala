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
class CanDiscoverMultipleMains extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProjectWithMultipleMain("multiMainDiscover")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  try {
    Await.result(child ? MainClassRequest(sendEvents = false), timeout.duration) match {
      case MainClassResponse(Seq(
        protocol.DiscoveredMainClasses(_, Seq("Main1", "Main2", "Main3"), _)
        )) =>
      case whatever => throw new AssertionError("unexpected result sending DiscoveredMainClassesRequest to app with multi main method: " + whatever)
    }
  } finally {
    system.stop(child)
  }
}