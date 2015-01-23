package com.typesafe.sbtrc
package it
package loading

import sbt.client._
import sbt.protocol._
import sbt.protocol.CoreProtocol._
import java.util.concurrent.Executors
import concurrent.duration.Duration.Inf
import concurrent.{ Await, ExecutionContext }
import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import annotation.tailrec

class CanKillServer extends SbtClientTest {
  def sleepUntilDead(client: SbtClient): Unit = {
    var count = 0
    while (!client.isClosed && count < 7) {
      val secondsToWait = (count * 2) + 1
      System.out.println(s"$count: Client has not closed yet, so sleeping until death (${secondsToWait} seconds).")
      Thread.sleep(secondsToWait * 1000L)
      count += 1
    }

    if (client.isClosed)
      System.out.println("Looks like client has closed!")
    else
      System.out.println(s"Giving up after $count waits - client never closed")
  }

  // Exit via "exit" command
  val dummy1 = utils.makeDummySbtProject("testExit")
  withSbt(dummy1) { client =>
    import concurrent.ExecutionContext.Implicits.global
    client.watchBuild { build =>
      System.out.println("Telling sbt to exit via exit command.")
      client.requestExecution("exit", interaction = None)
    }

    sleepUntilDead(client)

    assert(client.isClosed, s"Did not lose connection to sbt server which was supposed to be closed after exit")
  }

  // "reboot" command
  // from a client perspective, reboot looks just like quitting,
  // but on the server side it theoretically optimizes by
  // keeping the old JVM.
  val dummy2 = utils.makeDummySbtProject("testReboot")
  withSbt(dummy2) { client =>
    import concurrent.ExecutionContext.Implicits.global
    client.watchBuild { build =>
      System.out.println("Telling sbt to reboot.")
      client.requestExecution("reboot", interaction = None)
    }

    sleepUntilDead(client)

    assert(client.isClosed, s"Did not lose connection to sbt server which was supposed to be closed after reboot")
  }

  // exit via requestSelfDestruct() which we probably don't need anymore.
  val dummy3 = utils.makeDummySbtProject("testKill")
  withSbt(dummy3) { client =>
    import concurrent.ExecutionContext.Implicits.global
    client.watchBuild { build =>
      System.out.println("Telling sbt to die via special kill request.")
      client.requestSelfDestruct()
    }

    sleepUntilDead(client)

    assert(client.isClosed, s"Did not lose connection to sbt server which was supposed to be closed after requestSelfDestruct")
  }
}
