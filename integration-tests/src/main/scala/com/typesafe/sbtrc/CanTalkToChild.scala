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
abstract class AbstractCanTalkToChild(sbtVersion: String) extends SbtProcessLauncherTest {
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
class CanTalkToSbt12Child extends AbstractCanTalkToChild("0.12.3")