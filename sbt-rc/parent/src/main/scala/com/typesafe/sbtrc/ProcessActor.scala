package com.typesafe.sbtrc

import akka.actor._
import java.util.concurrent.Executors
import scala.collection.JavaConverters._
import java.io.File
import akka.util.ByteString
import java.util.concurrent.TimeUnit
import java.util.concurrent.CountDownLatch
import java.io.IOException
import akka.event.Logging
import java.io.InputStream
import java.io.BufferedInputStream
import java.util.concurrent.RejectedExecutionException
import scala.concurrent.duration._
import scala.annotation.tailrec

sealed trait ProcessRequest
case class SubscribeProcess(ref: ActorRef) extends ProcessRequest
case class UnsubscribeProcess(ref: ActorRef) extends ProcessRequest
case object StartProcess extends ProcessRequest
case object KillProcess extends ProcessRequest
case object FlushBuffer extends ProcessRequest

sealed trait ProcessEvent
case class ProcessStdOut(bytes: ByteString) extends ProcessEvent
case class ProcessStdErr(bytes: ByteString) extends ProcessEvent
case class ProcessStopped(exitValue: Int) extends ProcessEvent

class ProcessActor(argv: Seq[String], cwd: File, textMode: Boolean = true) extends Actor with ActorLogging {
  var subscribers: Set[ActorRef] = Set.empty
  var process: Option[Process] = None
  // flag handles the race where we get a kill request
  // before the process starts
  var killRequested = false

  var flushScheduled = false

  var eventBuffer = List.empty[ProcessEvent]

  val pool = NamedThreadFactory.newPool("ProcessActor")

  // counts down when we've read stdout and stderr
  val gotOutputLatch = new CountDownLatch(2)

  val selfRef = context.self

  override def receive = {
    case req: ProcessRequest => req match {
      case SubscribeProcess(ref) =>
        subscribers = subscribers + ref
        context.watch(ref)
      case UnsubscribeProcess(ref) =>
        subscribers = subscribers - ref
        context.unwatch(ref)
      case StartProcess =>
        if (killRequested)
          self ! ProcessStopped
        else
          start()
      case KillProcess =>
        killRequested = true
        // here we want to stop the process but NOT kill the actor
        // prematurely if the process fails to die. Since we only
        // send TERM not KILL on Linux, it could well fail to die.
        process.foreach { _.destroy() }
      case FlushBuffer =>
        flushScheduled = false
        flushBuffer()
    }

    case Terminated(ref) =>
      subscribers = subscribers - ref

    case event: ProcessEvent =>
      bufferEvent(event)

    case p: Process =>
      process = Some(p)
      if (killRequested) {
        p.destroy()
      }
  }

  private def bufferEvent(event: ProcessEvent): Unit = {
    if (log.isDebugEnabled) {
      log.debug("Buffering process event {}", event)
    }

    if (!flushScheduled) {
      // timeout length is intended to be user-imperceptible, but greater than OS timer resolution,
      // so we don't get the message instantly. Remember user perception of slow is 100ms
      // (rule of thumb).
      context.system.scheduler.scheduleOnce(Duration(30, TimeUnit.MILLISECONDS), self, FlushBuffer)(context.system.dispatcher)
      flushScheduled = true
    }
    eventBuffer = eventBuffer :+ event

    // flush buffer on a clean newline, this is pure heuristic in
    // case the process writes out lines all at once
    event match {
      case ProcessStdOut(bytes) if bytes.last == '\n' =>
        flushBuffer()
      case ProcessStdErr(bytes) if bytes.last == '\n' =>
        flushBuffer()
      case _ =>
    }
  }

  // the point of the buffering is to try to get logically-related output
  // bunched up into single messages, probabilistically, so we don't end
  // up with funny line breaks in the UI
  @tailrec
  private def flushBuffer(): Unit = {
    eventBuffer match {
      case ProcessStdOut(bytes1) :: ProcessStdOut(bytes2) :: tail =>
        eventBuffer = ProcessStdOut(bytes1 ++ bytes2) :: tail
        flushBuffer()
      case ProcessStdErr(bytes1) :: ProcessStdErr(bytes2) :: tail =>
        eventBuffer = ProcessStdErr(bytes1 ++ bytes2) :: tail
        flushBuffer()
      case event :: tail =>
        eventBuffer = tail
        for (s <- subscribers) {
          s ! event
        }
        event match {
          case ProcessStopped(_) =>
            context.stop(self)
          case _ =>
        }

        flushBuffer()
      case Nil =>
      // nothing to do
    }
  }

  def start(): Unit = {
    log.debug("Starting process with argv={}", argv)

    val pb = (new ProcessBuilder(argv.asJava)).directory(cwd)
    val process = pb.start()

    // we don't want the process to block on stdin.
    // redirecting stdin from /dev/null would be nicer than
    // closing it, but Java doesn't seem to have a way to do that.
    loggingFailure(log) { process.getOutputStream().close() }

    selfRef ! process

    def startReader(label: String, rawStream: InputStream, wrap: ByteString => ProcessEvent): Unit = try {
      pool.execute(new Runnable() {
        override def run = {
          val stream = new BufferedInputStream(rawStream)
          try {
            val bytes = new Array[Byte](4096)
            var eof = false
            while (!eof) {
              val count = stream.read(bytes)
              if (count > 0) {
                selfRef ! wrap(ByteString.fromArray(bytes, 0, count))
              } else if (count == -1) {
                eof = true
              }
            }
          } catch {
            case e: IOException =>
              // an expected exception here is "stream closed"
              // on stream close we end the thread.
              log.debug("    stream std{} from process closed", label)
          } finally {
            gotOutputLatch.countDown()
            log.debug("    ending std{} reader thread", label)
            try stream.close() catch { case e: IOException => }
          }
        }
      })
    } catch {
      case e: RejectedExecutionException =>
        log.warning("thread pool destroyed before we could read std" + label)
    }

    startReader("out", process.getInputStream(), ProcessStdOut)
    startReader("err", process.getErrorStream(), ProcessStdErr)

    def collectProcess(): Unit = {
      log.debug("  waiting for process")
      val result = process.waitFor()
      log.debug("  process waited for, waiting to gather any output")
      // try to finish reading out/in before we send ProcessStopped
      gotOutputLatch.await(5000, TimeUnit.MILLISECONDS)

      selfRef ! ProcessStopped(result)
    }

    try {
      pool.execute(new Runnable() {
        override def run = {
          log.debug("  process thread starting")
          collectProcess()
          log.debug("  process thread ending")
        }
      })
    } catch {
      case e: RejectedExecutionException =>
        log.warning("thread pool destroyed before we could wait for process")
        // at this point the process should be dead so this shouldn't block long
        collectProcess()
    }
  }

  override def postStop() = {
    log.debug("postStop")

    process.foreach { p =>

      process = None

      log.debug("  stopping process")

      // this unfortunately is a TERM not a KILL on Unix.
      // no way in Java (short of forking shell commands)
      // to do a KILL.
      p.destroy()

      // force-kill the streams in case that helps things
      // to die. Delay it so we have a chance to read a little
      // bit more.
      implicit val ec = context.system.dispatcher
      context.system.scheduler.scheduleOnce(2.seconds) {
        try p.getInputStream().close() catch { case e: IOException => }
        try p.getErrorStream().close() catch { case e: IOException => }
      }

      // briefly pause if we haven't read the stdout/stderr yet,
      // to just have a chance to get them. Note that this delay
      // is slightly longer than the scheduled stream close above.
      gotOutputLatch.await(3, TimeUnit.SECONDS)
    }

    // if we haven't started up the stdout/err reader or waitFor threads
    // yet, at this point they would get RejectedExecutionException
    if (!pool.isShutdown())
      pool.shutdown()
    if (!pool.isTerminated())
      pool.awaitTermination(2, TimeUnit.SECONDS)

    flushBuffer()
  }
}

object ProcessActor {
  def apply(system: ActorSystem, name: String, argv: Seq[String], cwd: File): ActorRef = {
    system.actorOf(Props(new ProcessActor(argv, cwd)), name)
  }
}
