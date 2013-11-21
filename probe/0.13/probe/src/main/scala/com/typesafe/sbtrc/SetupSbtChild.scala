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

object SetupSbtChild extends AbstractServerCommand("0.13") {
  import SbtUtil._

  override protected def installPluginShims(s: State): Boolean = {
    val atmosShims = controller.AtmosSupport.getAtmosShims(s)
    val ideShims = io.ShimWriter.sbt13ideShims
    controller.installRawShims(s, atmosShims ++ ideShims)
  }

  override protected def handleRequest(s: State, context: ui.Context, request: protocol.Request): RequestResult =
    controller.RequestHandler.handleRequest(s, context, request)

  // TODO - Is this correct?  maybe we should remove the previous UI context setting, or overwrite it.
  override protected def installRequestShims(serial: Long, context: ui.Context, state: State): State =
    reloadWithUiContext(state, context)

  override protected def installGlobalShims(state: State): State = {
    val s1 = disableSelectMain(state)
    val s2 = addTestListener(s1)
    val s3 = controller.AtmosSupport.installAtmosSupport(s2)
    controller.PlaySupport.installPlaySupport(s3)
  }

  // TODO - We should probably move all this hackery/shim stuff in one location that's
  // not in the same class as the server command.

  // Helper to turn off UI interaction for the main state.
  private def disableSelectMain(state: State): State = {
    val (extracted, ref) = extractWithRef(state)
    // this is supposed to get rid of asking on stdin for the main class,
    // instead just picking the first one.
    val pickFirstMainClass: Setting[_] =
      Keys.selectMainClass in Compile := {
        (Keys.mainClass in Compile).value orElse (Keys.discoveredMainClasses in Compile).value.headOption
      }
    val settings = makeAppendSettings(Seq(pickFirstMainClass), ref, extracted)
    reloadWithAppended(state, settings)
  }

  // Helpers to install test listening event reporters.
  private val uiTestListener = sbt.taskKey[sbt.TestReportListener]("The ui-event-sending report listener")
  private val testListenersKey = sbt.Keys.testListeners in Test in Global
  private def addTestListener(state: State): State = {
    PoorManDebug.trace("Adding test listener to state.")
    val (extracted, ref) = extractWithRef(state)
    val rawSettings = Seq(
      uiTestListener in Global := {
        new controller.UiTestListener((ui.SbtUiPlugin.uiContext in Global).value)
      },
      testListenersKey += (uiTestListener in Global).value)
    val settings = makeAppendSettings(rawSettings, ref, extracted)
    reloadWithAppended(state, settings)
  }
}
