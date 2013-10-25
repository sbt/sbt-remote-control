package com.typesafe.sbt.ui

import scala.util.parsing.json.JSONObject
import scala.util.parsing.json.Parser
import sbt.State

sealed trait Status
// if you get this, you MUST call either handle or sendError, but not both.
case class Request[RequestType, ResponseType](requst: RequestType, handle: (State, (State, Context,RequestType) => (State, ResponseType)) => State, sendError: String => Unit) extends Status
// you get this if you block for a message and there's no UI to get them from
case object NoUIPresent extends Status
// cancellation was just requested for the current task
// (this is an event which makes isCanceled state true)
case object Canceled extends Status

sealed trait Progress
case class MeasurableProgress(completed: Int, total: Int) extends Progress
// we are just "busy" (spinner) with unknown completed/total ratio
case object BusyProgress extends Progress

// when running a task on behalf of the UI, we provide this context.
// there's a different context for every task invocation.
// If there's no UI then we provide a no-op null version.
trait Context {
  // if true, it's OK/desired to give up. Task would typically
  // throw an exception in order to do this but in theory it
  // could also just complete.
  def isCanceled: Boolean

  // sets current progress; if percent is None we just want a "busy"
  // spinner since we don't know the length of time; if status is
  // non-None there's a message displayed (e.g. "Compiling foo.scala")
  def updateProgress(progress: Progress, status: Option[String] = None): Unit

  def sendEvent(id: String, event: Map[String,Any]): Unit

  // block for any messages during the task, if your task
  // can process messages.
  def take(): Status

  // check for messages without blocking, None if no
  // messages are currently available or there's no UI.
  def peek(): Option[Status]
}

object Context {
  // a UIContext that can be provided whenever there isn't a UI
  val noop = new Context() {
    override def isCanceled = false
    override def updateProgress(progress: Progress, status: Option[String]) = {}
    override def sendEvent(id: String, event: Map[String,Any]) = {}
    override def take(): Status = NoUIPresent
    override def peek(): Option[Status] = None
    override def toString: String = "NoopUiContext"
  }
}
