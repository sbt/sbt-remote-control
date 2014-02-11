package com.typesafe.sbtrc
package controller

import sbt._

object EchoSupport {
  val EchoTracePort = "echo-trace-port"

  val echoTracePortKey = TaskKey[Int](EchoTracePort)

  def makeTracePortSettings(key: sbt.ScopedKey[_], port: Int): Seq[Setting[_]] =
    Seq(echoTracePortKey in key.scope := port)

  def findEchoKey(name: String, settings: Seq[Setting[_]]): Option[ScopedKey[_]] =
    (for {
      setting <- settings
      if setting.key.key.label == name
    } yield setting.key).headOption

  def setTracePort(state: State, port: Int): State = {
    PoorManDebug.debug("Setting echo trace port.")
    val (extracted, ref) = SbtUtil.extractWithRef(state)
    val settings = extracted.session.mergeSettings
    val tracePortKey = findEchoKey(EchoTracePort, settings).getOrElse(
      sys.error("Unable to find echo trace port!  Possibly incompatible sbt-echo version."))
    val tracePortSettings = makeTracePortSettings(tracePortKey, port)
    val newSettings = SbtUtil.makeAppendSettings(tracePortSettings, ref, extracted)
    SbtUtil.reloadWithAppended(state, newSettings)
  }

  def isEchoProject(state: State): Boolean = {
    PoorManDebug.trace("Checking if sbt-echo is enabled.")
    val extracted = Project.extract(state)
    val settings = extracted.session.mergeSettings
    findEchoKey(EchoTracePort, settings).isDefined
  }

  def installEchoSupport(origState: State, tracePort: Option[Int]): State = {
    if (tracePort.isDefined && isEchoProject(origState)) {
      setTracePort(origState, tracePort.get)
    } else origState
  }
}
