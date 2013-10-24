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
class CanTalkToSbtOldChild extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProject("talkToChild", sbtVersion = "0.12.3")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)

  try {
    val name = Await.result(child ? NameRequest(sendEvents = false), timeout.duration) match {
      case NameResponse(n, _) => n
    }
    assertEquals("talkToChild", name)

    val name2 = Await.result(child ? NameRequest(sendEvents = false), timeout.duration) match {
      case NameResponse(n, _) => n
    }
    assertEquals("talkToChild", name2)

  } finally {
    system.stop(child)
  }
}