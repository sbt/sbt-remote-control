package com.typesafe.sbtrc
package client

import xsbti.{ AppMain, AppConfiguration }
import scala.concurrent.ExecutionContext
import sbt.client.{
  Interaction,
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

  object ReadContext extends ExecutionContext {
    def execute(runnable: Runnable): Unit = schedule(runnable)
    def reportFailure(t: Throwable): Unit = ()
  }

  object RunOnSameThreadContext extends ExecutionContext {
    def execute(runnable: Runnable): Unit = runnable.run()
    def reportFailure(t: Throwable): Unit = ()
  }
  val inStream = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))

  object TerminalInteraction extends Interaction {
    def readLine(prompt: String, mask: Boolean): Option[String] = {
      val maskChar = if (mask) Some('*') else None
      sbt.SimpleReader.readLine(prompt, maskChar)
    }

    def confirm(msg: String): Boolean = {
      object Assent {
        def unapply(in: String): Boolean = {
          (in == "y" || in == "yes")
        }
      }
      sbt.SimpleReader.readLine(msg + " (yes/no): ", None) match {
        case Some(Assent()) => true
        case _ => false
      }
    }
  }

  case class TakeNextCommand(client: SbtClient, reader: JLine) extends Runnable {
    override final def run(): Unit = try {
      reader.readLine("> ", None) match {
        case Some("exit") => System.exit(0)
        case None => run()
        case Some(line) =>
          // Register for when the execution is done.
          val executionDone = concurrent.promise[Unit]
          val registration = (client.handleEvents {
            case protocol.ExecutionDone(`line`) => executionDone.success(())
            case _ =>
          })(RunOnSameThreadContext)
          val started = client.requestExecution(line, Some(TerminalInteraction -> ReadContext))
          // Here we wait for the result of both starting (or failure) and the completion of the command.
          ((started zip executionDone.future).onComplete { _ =>
            registration.cancel()
            schedule(this)
          })(ReadContext)
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
    (connector onConnect { client =>
      import concurrent.ExecutionContext.global
      // This guy should handle future execution NOT on our event loop, or we'll block.
      // Ideally, "same thread" execution context instead.
      val reader = new sbt.terminal.RemoteJLineReader(None, client, true)(global)
      import protocol._
      (client handleEvents {
        case NowListeningEvent =>
          // Upon reconnection, down what's currently executing.
          clearAndSchedule(TakeNextCommand(client, reader))
        case LogEvent(LogSuccess(msg)) =>
          // TODO - ASCII CHARACTER CODES!
          reader.printLineAndRedrawPrompt(msg)
        case LogEvent(LogMessage(LogMessage.INFO, msg)) =>
          reader.printLineAndRedrawPrompt(s"[info] $msg")
        case LogEvent(LogMessage(LogMessage.WARN, msg)) =>
          reader.printLineAndRedrawPrompt(s"[warn] $msg")
        case LogEvent(LogMessage(LogMessage.ERROR, msg)) =>
          reader.printLineAndRedrawPrompt(s"[error] $msg")
        case LogEvent(LogStdOut(msg)) =>
          reader.printLineAndRedrawPrompt(msg)
        case LogEvent(LogStdErr(msg)) =>
          // TODO - on stderr?
          reader.printLineAndRedrawPrompt(msg)
        case _ => ()
      })(RunOnSameThreadContext)
    })(ReadContext)

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