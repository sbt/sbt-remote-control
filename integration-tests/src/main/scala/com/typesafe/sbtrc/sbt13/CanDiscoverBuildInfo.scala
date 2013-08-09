package com.typesafe.sbtrc
package sbt13

import com.typesafe.sbtrc.protocol._
import com.typesafe.sbtrc.it._
import java.io.File
import akka.actor._
import akka.pattern._
import akka.dispatch._
import concurrent.duration._
import concurrent.Await
import akka.util.Timeout
import akka.pattern.ask

/** Ensures that we can make requests and receive responses from our children. */
class CanDiscoverBuild extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProject("runChild13-idea", "0.13.0-RC4")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  try {
    Await.result(child ? SettingKeyRequest(KeyFilter.empty), timeout.duration) match {
      case KeyListResponse(keys) => // We succeeded!
        // TODO - Check the list
        keys.keys.foreach(println)
      case whatever => throw new AssertionError("did not get KeyListResponse got " + whatever)
    }
  } finally {
    system.stop(child)
  }
}