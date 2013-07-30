package com.typesafe.sbtrc

import com.typesafe.sbt.ui
import SbtUtil._
import com.typesafe.sbtrc.controller.ParamsHelper._

private[sbtrc] class ProbedContext(
  val serial: Long,
  val taskName: String,
  client: ipc.Client,
  blockForStatus: (ProbedContext) => ui.Status
) extends ui.Context {
    @volatile var cancelSerial = 0L
    override def isCanceled = cancelSerial != 0L
    override def updateProgress(progress: ui.Progress, status: Option[String]) = {} // TODO
    override def sendEvent(id: String, event: ui.Params) = {
      client.replyJson(serial, protocol.GenericEvent(task = taskName, id = id, params = event.toMap))
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