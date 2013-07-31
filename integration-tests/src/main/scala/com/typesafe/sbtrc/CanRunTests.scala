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

class CanRunTests extends SbtProcessLauncherTest {

  requestTest(utils.makeDummySbtProject("testing123")) { (child, context) =>
    implicit val self = context.self
    child ! TestRequest(sendEvents = true)
  } { results =>
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
      // yay!
      case whatever => throw new AssertionError("got wrong results: " + whatever)
    }
  }
}