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
abstract class AbstractCanTalkToChild(val sbtVersion: String) extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProject("talkToChild", sbtVersion = sbtVersion)
  val child = SbtProcess(system, dummy, sbtProcessLauncher)

  try {
    val name = Await.result(child ? NameRequest(sendEvents = false), timeout.duration) match {
      case NameResponse(Seq(ProjectInfo(_, n, _, _))) => n
    }
    assertEquals("talkToChild", name)

    val name2 = Await.result(child ? NameRequest(sendEvents = false), timeout.duration) match {
      case NameResponse(Seq(ProjectInfo(_, n, _, _))) => n
    }
    assertEquals("talkToChild", name2)

  } finally {
    system.stop(child)
  }
}
class CanTalkToSbt13Child extends AbstractCanTalkToChild("0.13.0")
// This is hardcoded to sbt 0.12.3 to ensure we're backwards compatible. 0.12.4 is the version
// that has the sbt hook we need to launch.  This ensures if the project is configured for
// 0.12.3, then 0.12.4+ is used.
class CanTalkToSbt12Child extends AbstractCanTalkToChild("0.12.3")