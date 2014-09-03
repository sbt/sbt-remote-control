package com.typesafe.sbtrc
package client

import xsbti.{ AppMain, AppConfiguration }
import scala.concurrent.{ ExecutionContext, Promise }
import sbt.client.{
  Interaction,
  SbtClient,
  SbtConnector,
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

  // Implements a trampolining runnable that will read a line of input, pass that to the
  // sbt server and wait for it to complete (successfully or otherwise) before registering
  // itself to run again.
  case class TakeNextCommand(client: SbtClient, reader: JLine) extends Runnable {
    override final def run(): Unit = try {
      reader.readLine("> ", None) match {
        case Some("exit") => System.exit(0)
        case None => run()
        case Some(line) =>
          val started = client.requestExecution(line, Some(TerminalInteraction -> ReadContext))
          // Here we wait for the result of both starting (or failure) and the completion of the command.
          val executionFuture = (started flatMap { executionId =>
            // Register for when the execution is done.
            val executionDone = Promise[Unit]
            // TODO this is broken because we add the event handler
            // AFTER we request execution, which means we might miss
            // the events. We need to add the event handler first
            // and then we are guaranteed to get events triggered
            // by a request we make after adding the handler.
            val registration = (client.handleEvents {
              case protocol.ExecutionSuccess(`executionId`) => executionDone.success(())
              case protocol.ExecutionFailure(`executionId`) =>
                // TODO - failure here?
                executionDone.success(())
              case _ =>
            })(RunOnSameThreadContext)
            executionDone.future.onComplete(_ => registration.cancel())(RunOnSameThreadContext)
            executionDone.future
          })(RunOnSameThreadContext)
          (executionFuture.onComplete { _ =>
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
    val connector = SbtConnector("terminal", "Command Line Terminal", configuration.baseDirectory)

    def onConnect(client: SbtClient): Unit = {
      // This guy should handle future execution NOT on our event loop, or we'll block.
      // Ideally, "same thread" execution context instead.
      val reader = new sbt.terminal.RemoteJLineReader(None, client, true)

      // Upon reconnection, down what's currently executing.
      clearAndSchedule(TakeNextCommand(client, reader))

      import protocol._
      (client handleEvents {
        case event: LogEvent => event.entry match {
          case LogSuccess(msg) =>
            // TODO - ASCII CHARACTER CODES!
            reader.printLineAndRedrawPrompt(msg)
          case LogMessage(LogMessage.INFO, msg) =>
            reader.printLineAndRedrawPrompt(s"[info] $msg")
          case LogMessage(LogMessage.WARN, msg) =>
            reader.printLineAndRedrawPrompt(s"[warn] $msg")
          case LogMessage(LogMessage.ERROR, msg) =>
            reader.printLineAndRedrawPrompt(s"[error] $msg")
          case LogMessage(_, _) => // debug or some weird unexpected string
          case LogTrace(exceptionClassName, msg) =>
            reader.printLineAndRedrawPrompt(s"[trace] $exceptionClassName: $msg")
          case LogStdOut(msg) =>
            reader.printLineAndRedrawPrompt(msg)
          case LogStdErr(msg) =>
            // TODO - on stderr?
            reader.printLineAndRedrawPrompt(msg)
        }
        case _ => ()
      })(RunOnSameThreadContext)
    }

    def onError(reconnecting: Boolean, message: String): Unit = {
      if (reconnecting) {
        System.out.println("Lost connection to sbt, reconnecting...")
      } else {
        System.out.println("Connection to sbt closed.")
        Exit(0)
      }
    }

    connector.open(onConnect, onError)(ReadContext)

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
