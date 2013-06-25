package com.typesafe.sbtrc

import scala.sys.process.Process
import java.io.File
import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import akka.util.ByteString

sealed trait SbtProcessRequest
case object KillSbtProcess extends SbtProcessRequest

// these are automatically done during a protocol.Request (subscribe on request,
// unsubscribe on response).
case class SubscribeOutput(ref: ActorRef) extends SbtProcessRequest
case class UnsubscribeOutput(ref: ActorRef) extends SbtProcessRequest

private class NeedRebootException extends Exception("Need to reboot")

class SbtProcessUnderlyingActor(supervisor: ActorRef, workingDir: File, sbtProcessMaker: SbtProcessLauncher) extends SafeWatchActor with ActorLogging {

  private val serverSocket = ipc.openServerSocket()
  private val port = serverSocket.getLocalPort()

  private val outDecoder = new ByteStringDecoder
  private val errDecoder = new ByteStringDecoder

  private var protocolStartedAndStopped = false
  // it appears that ActorRef.isTerminated is not guaranteed
  // to be true when we get the Terminated event, which means
  // we have to track these by hand.
  private var serverTerminated = false
  private var processTerminated = false
  private var needsToReboot = false

  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  // TODO the "textMode=true" here shouldn't be needed but scala 2.9.2 seems to not
  // realize that it has a default value? maybe some local quirk on my system?
  private val process = context.actorOf(Props(new ProcessActor(sbtProcessMaker.arguments(port),
    workingDir, textMode = true)), "sbt-process")

  private val server = context.actorOf(Props(new ServerActor(serverSocket, supervisor)), "sbt-server")

  watch(server)
  watch(process)

  server ! SubscribeServer(self)
  server ! StartServer
  process ! SubscribeProcess(self)
  process ! StartProcess

  private def considerSuicide(): Unit = {
    if (processTerminated) {
      if (serverTerminated) {
        log.debug("both server actor and process actor terminated, sbt child actor going away")
        if (needsToReboot) {
          // to be restarted by supervisor (discarding mailbox)
          throw new NeedRebootException
        } else {
          // graceful shutdown with no restart (and empty our mailbox first)
          self ! PoisonPill
        }
      } else if (protocolStartedAndStopped) {
        log.debug("stopped message received from socket to child and child process is dead, killing socket")
        server ! PoisonPill
      }
    } else if (protocolStartedAndStopped) {
      log.debug("stopped message received from socket but process still alive, killing process")
      process ! PoisonPill
    }
  }

  override def onTerminated(ref: ActorRef): Unit = {
    super.onTerminated(ref)
    if (ref == process) {
      processTerminated = true
      // the socket will never connect, if it hasn't.
      // we don't want accept() to perma-block
      log.debug("closing server socket because process exited")
      if (!serverSocket.isClosed())
        serverSocket.close()
      considerSuicide()
    } else if (ref == server) {
      serverTerminated = true
      considerSuicide()
    }
  }

  private def forwardRequest(requestor: ActorRef, req: protocol.Request): Unit = {
    // checking isTerminated here is a race, but when the race fails the sender
    // should still time out. We're just trying to short-circuit the timeout if
    // we know it will time out.
    if (serverTerminated || processTerminated || protocolStartedAndStopped) {
      log.debug("Got request {} on already-shut-down ServerActor", req)
      requestor ! protocol.ErrorResponse("sbt has already shut down")
    } else {
      // otherwise forward the request
      server.tell(req, requestor)
    }
  }

  override def receive = {
    case Terminated(ref) =>
      onTerminated(ref)

    case req: SbtProcessRequest => req match {
      case SubscribeOutput(ref) => throw new RuntimeException("supervisor should have handled subscribe")
      case UnsubscribeOutput(ref) => throw new RuntimeException("supervisor should have handled unsubscribe")
      case KillSbtProcess => {
        // synthesize a nice message for the logs
        supervisor ! protocol.LogEvent(protocol.LogMessage(protocol.LogMessage.INFO, "Attempting to stop SBT process"))
        // now try to kill it
        process ! KillProcess
      }
    }

    // request for the server actor
    case req: protocol.Request =>
      forwardRequest(sender, req)

    // message from server actor other than a response
    case event: protocol.Event =>
      event match {
        case protocol.NeedRebootEvent =>
          needsToReboot = true
          considerSuicide()
        case protocol.NowListeningEvent =>
          throw new RuntimeException("NowListeningEvent isn't supposed to be forwarded from ServerActor")
        case protocol.Started =>
          supervisor ! protocol.Started
        case protocol.Stopped =>
          log.debug("server actor says it's all done, killing it")
          protocolStartedAndStopped = true
          server ! PoisonPill
        case e: protocol.LogEvent =>
          // this is a log event outside of the context of a particular request
          // (if they go with a request they just go to the requestor)
          supervisor ! e
        case protocol.RequestReceivedEvent =>
          throw new RuntimeException("Not expecting a RequestReceivedEvent here")
        case e: protocol.TestEvent =>
          throw new RuntimeException("Not expecting a TestEvent here: " + e)
        case protocol.MysteryMessage(something) =>
          // let it crash
          throw new RuntimeException("Received unexpected item on socket from sbt child: " + something)
        case e: protocol.GenericMessage =>
          throw new RuntimeException("not expecting generic messages here yet: " + e)
      }

    // event from process actor
    case event: ProcessEvent =>

      def handleOutput(label: String, decoder: ByteStringDecoder, entryMaker: String => protocol.LogEntry, bytes: ByteString): Unit = {
        decoder.feed(bytes)
        val s = decoder.read.mkString
        if (s.length > 0) {
          s.split("\n") foreach { line =>
            log.debug("sbt {}: {}", label, line)
            supervisor ! protocol.LogEvent(entryMaker(line))
          }
        }
      }

      event match {
        case ProcessStopped(status) =>
          // we don't really need this event since ProcessActor self-suicides
          // and we get Terminated
          log.debug("sbt process stopped, status: {}", status)
        case ProcessStdOut(bytes) =>
          handleOutput("out", outDecoder, protocol.LogStdOut.apply, bytes)
        case ProcessStdErr(bytes) =>
          handleOutput("err", errDecoder, protocol.LogStdErr.apply, bytes)
      }
  }

  override def postStop() = {
    log.debug("postStop")

    if (!serverSocket.isClosed())
      serverSocket.close()
  }
}

// we track the pre-start buffer and event subscriptions, since
// those are supposed to persist across restart
class SbtProcessSupervisorActor(workingDir: File, sbtLauncher: SbtProcessLauncher) extends EventSourceActor with ActorLogging {
  private var protocolStarted = false
  private var preStartBuffer = Vector.empty[(protocol.Request, ActorRef)]
  private val underlying = context.actorOf(Props(new SbtProcessUnderlyingActor(self, workingDir, sbtLauncher)),
    name = "underlying")

  watch(underlying)

  // restart only on NeedRebootException
  import akka.actor.SupervisorStrategy
  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 1, withinTimeRange = Duration(60, TimeUnit.SECONDS)) {
    case e: NeedRebootException =>
      if (protocolStarted) {
        log.error("Should be impossible to get protocol.Started if SBT had to reboot")
        SupervisorStrategy.Stop
      } else {
        log.debug(s"Restarting SBT actor due to ${e.getClass.getName}")
        SupervisorStrategy.Restart
      }
    case e =>
      log.warning(s"Fatal error to underlying SBT actor ${e.getClass.getName}: ${e.getMessage}")
      SupervisorStrategy.Stop
  }

  override def onTerminated(ref: ActorRef): Unit = {
    super.onTerminated(ref)
    // if underlying dies for good (vs. is restarted) we do this
    if (ref == underlying) {
      log.debug("underlying actor terminated, sending PoisonPill to self")
      self ! PoisonPill
    }
  }

  private def forwardRequest(requestor: ActorRef, req: protocol.Request): Unit = {
    if (protocolStarted) {
      underlying.tell(req, requestor)
    } else {
      log.debug("storing request for when server gets a connection {}", req)
      preStartBuffer = preStartBuffer :+ (req, sender)
    }
  }

  override def receive = {
    case Terminated(ref) =>
      onTerminated(ref)

    case req: SbtProcessRequest => req match {
      case SubscribeOutput(ref) => subscribe(ref)
      case UnsubscribeOutput(ref) => unsubscribe(ref)
      case KillSbtProcess => underlying.forward(req)
    }

    // request for the server actor
    case req: protocol.Request =>
      // auto-subscribe to stdout/stderr; ServerActor is then responsible
      // for sending an Unsubscribe back to us when it sends the Response.
      // Kind of hacky. If ServerActor never starts up, we will stop ourselves
      // and so we don't need to unsubscribe our listeners. We want to subscribe
      // right away, not just when the server starts, because we want sbt's startup
      // messages.
      if (req.sendEvents)
        subscribe(sender)

      // now send the request on (or buffer it)
      forwardRequest(sender, req)

    // here we get events that didn't have a reply serial
    // (weren't part of a specific request) and forward them
    case event: protocol.Event =>
      event match {
        case protocol.Started =>
          protocolStarted = true
          preStartBuffer foreach { m =>
            // We want the requestor to know the boundary between sbt
            // startup and it connecting to us, so it can probabilistically
            // ignore startup messages for example.
            // We do NOT send this event if the request arrives after sbt
            // has already started up, only if the request's lifetime
            // includes an sbt startup.
            if (isSubscribed(m._2))
              m._2.forward(event)
            forwardRequest(m._2, m._1)
          }
          preStartBuffer = Vector.empty
        case event: protocol.LogEvent =>
          emitEvent(event)
        case other =>
          log.error("Not expecting event here {}", other)
      }
  }

  override def postStop() = {
    log.debug("postStop")

    preStartBuffer foreach { m =>
      log.debug("On destroy, replying with error to queued request that never made it to the socket {}, requestor.isTerminated={}",
        m._1, m._2.isTerminated)
      m._2 ! protocol.ErrorResponse("sbt process never got in touch, so unable to handle request " + m._1)
    }
    preStartBuffer = Vector.empty
  }
}

object SbtProcess {
  def apply(factory: ActorRefFactory, workingDir: File, sbtLauncher: SbtProcessLauncher): ActorRef =
    factory.actorOf(Props(new SbtProcessSupervisorActor(workingDir, sbtLauncher)),
      "sbt-process-" + SbtProcess.nextSerial.getAndIncrement())

  private val nextSerial = new AtomicInteger(1)
}
