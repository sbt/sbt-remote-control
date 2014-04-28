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
  withSbt(dummy) { client =>
    import concurrent.ExecutionContext.Implicits.global
    client.watchBuild { build =>
      System.err.println("Telling sbt to die.")
      client.attemptToReboot()
    }
    Thread.sleep(30 * 1000L)
    // TODO - Check to ensure this client Was closed...

  }
}