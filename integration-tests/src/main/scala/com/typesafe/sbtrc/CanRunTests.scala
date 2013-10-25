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

abstract class CanRunTests(val sbtVersion: String) extends SbtProcessLauncherTest {

  requestTest(utils.makeDummySbtProject("testing123", sbtVersion)) { (child, context) =>
    implicit val self = context.self
    child ! TestRequest(sendEvents = true)
  } { results =>
    // 0.12 and 0.13 give us somewhat different TestEvent
    noLogs(results).sorted match {
      case Seq(Started,
        RequestReceivedEvent,
        TestEvent("OneFailTest.testThatShouldFail",
          Some("this is not true"), TestFailed, Some("this is not true")),
        TestEvent("OnePassOneFailTest.testThatShouldFail",
          Some("this is not true"), TestFailed, Some("this is not true")),
        TestEvent("OnePassOneFailTest.testThatShouldPass", None, TestPassed, None),
        TestEvent("OnePassTest.testThatShouldPass", None, TestPassed, None),
        ErrorResponse("exception during sbt task: test: Incomplete: null")) =>
        if (!sbtVersion.startsWith("0.12"))
          throw new AssertionError("0.12-like results obtained for " + sbtVersion)
      case Seq(Started,
        RequestReceivedEvent,
        TestEvent("OneFailTest",
          None, TestFailed, Some("this is not true")),
        TestEvent("OnePassOneFailTest",
          None, TestFailed, Some("this is not true")),
        TestEvent("OnePassOneFailTest",
          None, TestPassed, None),
        TestEvent("OnePassTest", None, TestPassed, None),
        ErrorResponse("exception during sbt task: test: Incomplete: null")) =>
        if (!sbtVersion.startsWith("0.13"))
          throw new AssertionError("0.13-like results obtained for " + sbtVersion)
      case whatever => throw new AssertionError("got wrong results: " + whatever)
    }
  }
}

class CanRunTestsSbt12 extends CanRunTests("0.12.4")

class CanRunTestsSbt13 extends CanRunTests("0.13.0")
