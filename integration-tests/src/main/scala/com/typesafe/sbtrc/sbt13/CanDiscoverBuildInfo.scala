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
  val dummy = utils.makeDummySbtProject("runTestBuild", "0.13.0-RC4")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  object KeyListExtract {
    def unapply(params: Map[String, Any]): Option[KeyList] =
      Parametize.unapply[KeyList](params)
  }
  try {
    val request = SettingKeyRequest(KeyFilter.empty)
    Await.result(child ? request, timeout.duration) match {
      case protocol.KeyListResponse(keyList) => // We succeeded!
        // TODO - Check the list
        keyList.keys.foreach(println)
        assert(keyList.keys.size > 0, "Key list must not be zero")
      case whatever => throw new AssertionError("did not get KeyListResponse got " + whatever)
    }
  } finally {
    system.stop(child)
  }
}