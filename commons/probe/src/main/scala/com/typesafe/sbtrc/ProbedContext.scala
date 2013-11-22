package com.typesafe.sbtrc

import com.typesafe.sbt.ui
import SbtUtil._

private[sbtrc] class ProbedContext(
  val serial: Long,
  val taskName: String,
  client: ipc.Client,
  blockForStatus: (ProbedContext) => ui.Status
) extends ui.Context {
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