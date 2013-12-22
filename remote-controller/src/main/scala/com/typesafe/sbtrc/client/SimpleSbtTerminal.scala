package com.typesafe.sbtrc
package client

import xsbti.{ AppMain, AppConfiguration }
import scala.concurrent.ExecutionContext
import com.typesafe.sbtrc.api.SbtClient

class SimpleSbtTerminal extends xsbti.AppMain {
  private var queue = new java.util.concurrent.LinkedBlockingDeque[Runnable]
  private def schedule(run: Runnable): Unit = queue.add(run)

  implicit object Context extends ExecutionContext {
    def execute(runnable: Runnable): Unit =
      schedule(runnable)
    def reportFailure(t: Throwable): Unit = ()
  }
  val inStream = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
  case class TakeNextCommand(client: SbtClient) extends Runnable {
    override final def run(): Unit = {
      System.out.print("> ")
      inStream.readLine match {
        case "exit" => System.exit(0)
        case null => run()
        case line =>
          // TODO - We want to wait to schedule the next execution until after this
          // command is done...
          client.requestExecution(line)
          // Read another line
          schedule(this)
      }

    }
  }

  case class Exit(code: Int) extends xsbti.Exit
  override def run(configuration: AppConfiguration): xsbti.Exit = {
    val connector = new SimpleConnector(configuration.baseDirectory, SimpleLocator)
    connector onConnect { client =>
      import protocol._
      client handleEvents {
        case LogEvent(LogStdOut(msg)) => System.out.print(msg)
        case LogEvent(LogStdErr(msg)) => System.err.print(msg)
        case _ => ()
      }
      schedule(TakeNextCommand(client))
    }

    // Now we need to run....
    def loop(): Unit = {
      val next = queue.take
      next.run()
      loop()
    }
    loop()

    Exit(0)
  }
}