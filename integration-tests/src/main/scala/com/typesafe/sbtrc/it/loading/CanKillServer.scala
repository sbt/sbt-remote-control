package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import java.util.concurrent.Executors
import concurrent.duration.Duration.Inf
import concurrent.{ Await, ExecutionContext }
import java.io.File
import sbt.client.ScopedKey
import java.util.concurrent.LinkedBlockingQueue
import annotation.tailrec

class CanKillServer extends SbtClientTest {
  val dummy = utils.makeDummySbtProject("test")
  val connects = withSbt(dummy) { client =>
    import concurrent.ExecutionContext.Implicits.global
    client.watchBuild { build =>
      System.out.println("Telling sbt to die.")
      client.requestSelfDestruct()
    }
    var count = 0
    while (!client.isClosed && count < 5) {
      val secondsToWait = (count * 3) + 1
      System.out.println(s"Client has not closed yet, so sleeping until death (${secondsToWait} seconds).")
      Thread.sleep(secondsToWait * 1000L)
      count += 1
    }

    assert(client.isClosed, s"Did not lose connection to sbt server which was supposed to be closed")
  }
}
