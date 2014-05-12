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
      if (setting.key.key.label == name) || (setting.key.key.rawLabel == name)
    } yield setting.key).headOption

  def setTracePort(state: State, port: Int): State = {
    System.err.println("Setting echo trace port.")
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

  import io.{ ShimWriter, GenericShimWriter }
  import ShimWriter.{
    echoPlayPluginShim,
    echoPlayBuildShim,
    echoPluginShim,
    echoAkkaBuildShim,
    echoPluginDeleteShim,
    echoAkkaBuildDeleteShim,
    echoPlayPluginDeleteShim,
    echoPlayBuildDeleteShim
  }

  def getEchoShims(state: State): Seq[ShimWriter] = {
    // TODO - Detect play/akka by project.
    val playSupported = PlaySupport.playVersion(state) map { version =>
      val ok = VersionCompare(version, BuildInfo.supportedPlayVersionSbt013) <= 0
      PoorManDebug.trace("Play version " + version + " required for Atmos " + BuildInfo.supportedPlayVersionSbt013 + " supported=" + ok)
      ok
    } getOrElse false
    val akkaSupported = AkkaSupport.akkaVersion(state) map { version =>
      val ok = VersionCompare(version, BuildInfo.supportedAkkaVersionSbt013) <= 0
      PoorManDebug.trace("Akka version " + version + " required for Atmos " + BuildInfo.supportedAkkaVersionSbt013 + " supported=" + ok)
      ok
    } getOrElse false

    // TODO - When we have the latest echo plugin we can include the build shim
    // TODO - We need a shim to turn off the echoBuildShim if an akka project migrates to play...
    if (playSupported && akkaSupported) {
      PoorManDebug.trace("Play+Echo hooks are needed.")
      Seq(echoPlayPluginShim, echoPlayBuildShim,
        // When installing Play support, make sure we delete Akka support,
        // or things get wonky.
        echoAkkaBuildDeleteShim, echoPluginDeleteShim)
    } else if (akkaSupported) {
      PoorManDebug.trace("Akka+Echo hooks are needed.")
      // We have to also delete Play echo support if migrating from
      // play -> just akka.
      Seq(echoPluginShim, echoAkkaBuildShim,
        echoPlayPluginDeleteShim, echoPlayBuildDeleteShim)
    } else {
      PoorManDebug.trace("No Echo hooks are needed.")
      Nil
    }
  }
}
