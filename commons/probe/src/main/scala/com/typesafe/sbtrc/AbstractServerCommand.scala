package com.typesafe.sbtrc

import _root_.sbt._
import Project.Initialize
import Keys.logManager
import Scope.GlobalScope
import sbt.Aggregation.KeyValue
import sbt.complete.DefaultParsers
import sbt.Load.BuildStructure
import java.net.SocketException
import java.io.EOFException
import java.io.IOException
import java.io.PrintWriter
import java.io.Writer
import scala.util.matching.Regex
import com.typesafe.sbt.ui
import scala.util.parsing.json._
import scala.annotation.tailrec
import SbtCustomHacks._
import com.typesafe.sbtrc.ipc.JsonWriter

sealed trait RequestResult
case class RequestSuccess(response: protocol.Response, state: State) extends RequestResult
case object RequestNotHandled extends RequestResult
case class RequestFailure(exception: Throwable) extends RequestResult

abstract class AbstractServerCommand(sbtProbeVersion: String) extends (State => State) {

  /** Installs the shims and tells us if any were added. 
   *
   *  Shims are sbt plugins that must be resolved/reloaded in order for the
   *  build to work correctly.  
   */
  protected def installPluginShims(s: State): Boolean
  
  /**
   * Installs any per-request shims that we need on the state so we can
   * fire back events.
   */
  protected def installRequestShims(serial: Long, context: ui.Context, state: State): State
  
  /**
   * Attempts to handle a user request.
   * 
   * @return One of three states:   RequestSuccess, RequestNotHandled or RequestFailure.
   */
  protected def handleRequest(s: State, context: ui.Context, request: protocol.Request): RequestResult
  
  import SbtUtil._

  private lazy val client = ipc.openClient(getPort())
  private var sentNowListening = false

  val ListenCommandName = "listen"

  // this is the entry point invoked by sbt
  override def apply(s: State): State = {
    try {
      val betweenRequestsLogger = new EventLogger(client, 0L)
      val loggedState = addLogger(s, betweenRequestsLogger.toGlobalLogging)

      // this property is set to true for unit tests but not integration
      // tests or production.
      if (System.getProperty("sbtrc.no-shims", "false") != "true") {
        // Make sure the shims are installed we need for this build.
        if (installPluginShims(loggedState)) {
          client.sendJson(protocol.NeedRebootEvent)
          // close down in orderly fashion
          client.close()
          // By erroring out (and doing so before responding to protocol method),
          // We force the sbt process to reload and try again...
          throw new NeedToRebootException
        }
      }

      // now add our command
      disableSelectMain(loggedState) ++ Seq(listen)
    } catch {
      case e: NeedToRebootException =>
        throw e
      case e: Exception =>
        // this is pure paranoia
        System.err.println("Failed to control sbt: " + e.getClass.getName + ": " + e.getMessage);
        e.printStackTrace()
        throw e
    }
  }

  private def addLogger(origState: State, logging: GlobalLogging): State = {
    addLogManager(origState.copy(globalLogging = logging), logging.full)
  }

  private def withLogger(origState: State, logging: GlobalLogging)(f: State => State): State = {
    // This never restores the original LogManager, for now it doesn't matter since
    // it does restore one that uses origState.globalLogging.full which will be the
    // logger we want.
    addLogger(f(addLogger(origState, logging)), origState.globalLogging)
  }

  private case class ContextIndifferentLogManager(logger: Logger) extends LogManager {
    override def apply(data: Settings[Scope], state: State, task: ScopedKey[_], writer: PrintWriter): Logger = logger
  }

  private def addLogManager(state: State, logger: Logger): State = {
    val (extracted, ref) = extractWithRef(state)

    val settings = makeAppendSettings(Seq(logManager := ContextIndifferentLogManager(logger)), ref, extracted)

    reloadWithAppended(state, settings)
  }

  // TODO - Why is this here and not in the handlers?  We may need to make this
  // abstract to support multiple sbt versions...
  private def disableSelectMain(state: State): State = {
    val (extracted, ref) = extractWithRef(state)

    // this is supposed to get rid of asking on stdin for the main class,
    // instead just picking the first one.

    val pickFirstMainClass: Setting[_] = 
      Keys.selectMainClass in Compile <<=
        (Keys.mainClass in Compile, Keys.discoveredMainClasses in Compile) map {
          (mc, discovered) =>
            mc orElse discovered.headOption
        }

    val settings = makeAppendSettings(Seq(pickFirstMainClass), ref, extracted)
    reloadWithAppended(state, settings)
  }

  @tailrec
  private def blockForStatus(inContext: ProbedContext): ui.Status = {
    val req = blockForRequest()
    req match {
      case protocol.Envelope(serial, replyTo, protocol.CancelRequest) =>
        if (inContext.cancelSerial != 0L) {
          client.replyJson(serial, protocol.ErrorResponse("Already canceled, received another cancel request"))
        } else {
          inContext.cancelSerial = serial
          client.replyJson(serial, protocol.RequestReceivedEvent)
          // we send CancelResponse when the context is closed (i.e. the task in fact exits)
        }
        ui.Canceled
      case protocol.Envelope(serial, replyTo, request: protocol.Request) =>
        ui.Request[protocol.Request](
          request = request, 
          handler = { (state, handler) =>
            handleRequestImpl(serial, request, state)
          }, 
          sendError = { error =>
            client.replyJson(serial, protocol.ErrorResponse(error))
          })
      case protocol.Envelope(serial, _, message) =>
        client.replyJson(serial, protocol.ErrorResponse("Message received while another request was active: " + message))
        blockForStatus(inContext)
    }
  }

  private class ProbedContext(val serial: Long, val taskName: String) extends ui.Context {
    @volatile var cancelSerial = 0L
    override def isCanceled = cancelSerial != 0L
    override def updateProgress(progress: ui.Progress, status: Option[String]) = {} // TODO
    override def sendEvent(id: String, event: Map[String, Any]) = {
      client.replyJson(serial, protocol.GenericEvent(id = id, params = event))
    }
    override def take(): ui.Status = {
      blockForStatus(this)
    }
    override def peek(): Option[ui.Status] = None // TODO we have no actual way to implement this right now
    override def toString: String = "ProbedContext(serial=" + serial + ", taskName =" + taskName + ")"
    def close(): Unit = {
      // send pending CancelResponse
      if (cancelSerial != 0L) {
        client.replyJson(cancelSerial, protocol.CancelResponse)
      }
    }
  }

  private def getPort(): Int = {
    val portString = System.getProperty("sbtrc.control-port")
    if (portString == null)
      throw new Exception("No port property set")
    val port = Integer.parseInt(portString)
    port
  }
  
  private def handleRequestImpl(serial: Long, request: protocol.Request, origState: State): State = {
    try {
      client.replyJson(serial, protocol.RequestReceivedEvent)
      val context = new ProbedContext(serial, request.simpleName)
      // TODO - Does this make sense here?  Maybe we should ensure
      // that we can remove the shims with a loaner-pattern....
      val newState = installRequestShims(serial, context, origState)
      
      try handleRequest(newState, context, request) match {
        case RequestNotHandled =>
          client.replyJson(serial, protocol.ErrorResponse("SBT " +sbtProbeVersion+" - No handler for: " + request.simpleName))
          origState
        case RequestFailure(err) =>
          // Catch this in outer catch block.
          // TODO - output via debugging...
          throw err
        case RequestSuccess(response, state) => 
          // Send the response, then return.
          import protocol.WireProtocol.jsonWriter
          client.replyJson(serial, response)
          state
      } finally {
        context.close()
      }
    } catch {
      case e: Exception =>
        client.replyJson(serial, protocol.ErrorResponse("exception during sbt task: " + request.simpleName + ": " + e.getClass.getSimpleName + ": " + e.getMessage))
        origState
    }
  }

  private def handleRequestEnvelope(req: protocol.Envelope, origState: State): State = {
    req match {
      case protocol.Envelope(serial, replyTo, protocol.CancelRequest) =>
        client.replyJson(serial, protocol.ErrorResponse("No active task to cancel"))
        origState
      case protocol.Envelope(serial, replyTo, request: protocol.Request) =>
        handleRequestImpl(serial, request, origState)
      case _ => {
        client.replyJson(req.serial, protocol.ErrorResponse("Unknown request: " + req))
        origState
      }
    }
  }

  private def blockForRequest(): protocol.Envelope = {
    try protocol.Envelope(client.receive()) catch {
      case e: IOException =>
        System.err.println("Lost connection to parent process: " + e.getClass.getSimpleName() + ": " + e.getMessage())
        System.exit(0)
        throw new RuntimeException("not reached") // compiler doesn't know that System.exit never returns
    }
  }

  val listen = Command.command(ListenCommandName, Help.more(ListenCommandName, "listens for remote commands")) { origState =>
    if (!sentNowListening) {
      sentNowListening = true
      client.sendJson(protocol.NowListeningEvent)
    }
    val req = blockForRequest()

    val newLogger = new EventLogger(client, req.serial)

    withLogger(origState, newLogger.toGlobalLogging) { loggedState =>
      val afterTaskState: State = handleRequestEnvelope(req, loggedState)

      val newState = afterTaskState.copy(onFailure = Some(ListenCommandName),
        remainingCommands = ListenCommandName +: afterTaskState.remainingCommands)
      newState
    }
  }

}
