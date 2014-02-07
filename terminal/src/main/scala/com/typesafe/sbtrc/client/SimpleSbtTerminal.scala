package com.typesafe.sbtrc
package client

import xsbti.{ AppMain, AppConfiguration }
import scala.concurrent.ExecutionContext
import sbt.client.{
  SbtClient,
  RemoteKeys,
  RemoteConfigurations
}
import sbt.JLine
import sbt.protocol

class SimpleSbtTerminal extends xsbti.AppMain {
  private var queue = new java.util.concurrent.LinkedBlockingDeque[Runnable]
  private def schedule(run: Runnable): Unit = queue.add(run)
  private def clearAndSchedule(run: Runnable): Unit = {
    queue.clear()
    schedule(run)
  }

  implicit object Context extends ExecutionContext {
    def execute(runnable: Runnable): Unit =
      schedule(runnable)
    def reportFailure(t: Throwable): Unit = ()
  }
  val inStream = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
  case class TakeNextCommand(client: SbtClient, reader: JLine) extends Runnable {
    override final def run(): Unit = try {
      reader.readLine("> ", None) match {
        case Some("exit") => System.exit(0)
        case None => run()
        case Some(line) =>
          // Register for when the execution is done.
          val executionDone = concurrent.promise[Unit]
          val registration = client.handleEvents {
            case protocol.ExecutionDone(`line`) => executionDone.success(())
            case _ =>
          }
          // TODO - We want to wait to schedule the next execution until after this
          // command is done...
          client.requestExecution(line, None)
          executionDone.future.onComplete { _ =>
            registration.cancel()
            schedule(this)
          }
      }
    } catch {
      case e: Exception =>
        // Here we want to ignore exceptions and make sure not to schedule more work.
        // We should probably print the exception if it's *NOT* a connection related issue that
        // will cause us to reconnect anyway....
        e.printStackTrace()
    }

  }

  case class Exit(code: Int) extends xsbti.Exit
  override def run(configuration: AppConfiguration): xsbti.Exit = {
    System.out.println("Connecting to sbt...")
    val connector = new SimpleConnector(configuration.baseDirectory, SimpleLocator)
    connector onConnect { client =>
      // This guy should handle future execution NOT on our event loop, or we'll block.
      // Ideally, "same thread" execution context instead.
      val reader = new sbt.terminal.RemoteJLineReader(None, client, true)(concurrent.ExecutionContext.global)
      import protocol._
      client handleEvents {
        case NowListeningEvent =>
          // Upon reconnection, down what's currently executing.
          clearAndSchedule(TakeNextCommand(client, reader))
        case LogEvent(LogSuccess(msg)) =>
          System.out.println()
          System.out.println(msg)
          System.out.flush()
        // TODO - ASCII CHARACTER CODES!
        case LogEvent(LogMessage(LogMessage.INFO, msg)) =>
          System.out.print(s"[info] $msg\n")
          System.out.flush()
        case LogEvent(LogMessage(LogMessage.WARN, msg)) =>
          System.out.print(s"[warn] $msg\n")
          System.out.flush()
        case LogEvent(LogMessage(LogMessage.ERROR, msg)) =>
          System.err.print(s"[error] $msg\n")
          System.err.flush()
        case LogEvent(LogStdOut(msg)) =>
          System.out.print(msg)
          System.out.flush()
        case LogEvent(LogStdErr(msg)) =>
          System.err.print(msg)
          System.err.flush()
        case _ => ()
      }
      client watchBuild { build =>
        val project = build.projects.head
        val key = RemoteKeys.fullClasspath in project in RemoteConfigurations.Compile
        client.watch(key) { (key, classpath) =>
          System.out.println("New classpath = " + classpath)
          System.out.flush()
        }
      }
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