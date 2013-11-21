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
import com.typesafe.sbtrc.controller.{ PlaySupport, AtmosSupport }
import scala.annotation.tailrec
import SbtCustomHacks._
import com.typesafe.sbtrc.ipc.JsonWriter

object SetupSbtChild extends AbstractServerCommand("0.12") {
  import SbtUtil._

  override def installPluginShims(s: State): Boolean = {
    controller.installShims(s)
  }

  override protected def handleRequest(s: State, context: ui.Context, request: protocol.Request): RequestResult =
    controller.RequestHandler.handleRequest(s, context, request)
  // TODO - Is this correct?  maybe we should remove the previous UI context setting, or overwrite it.
  override protected def installRequestShims(serial: Long, context: ui.Context, state: State): State =
    reloadWithUiContext(state, context)

  override protected def installGlobalShims(state: State): State = {
    val s1 = disableSelectMain(state)
    val s2 = addTestListener(s1)
    val s3 = PlaySupport.installPlaySupport(s2)
    AtmosSupport.installAtmosSupport(s3)
  }

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

  // Helpers to install test listening event reporters.
  private val uiTestListener = sbt.TaskKey[sbt.TestReportListener]("uiTestListener", "The ui-event-sending report listener")
  private val testListenersKey = sbt.Keys.testListeners in Test in Global
  private def addTestListener(state: State): State = {
    PoorManDebug.trace("Adding test listener to state.")
    val (extracted, ref) = extractWithRef(state)
    val rawSettings = Seq(
      uiTestListener in Global <<= (ui.SbtUiPlugin.uiContext in Global) map { ui =>
        new controller.UiTestListener(ui)
      },
      testListenersKey <+= (uiTestListener in Global))
    val settings = makeAppendSettings(rawSettings, ref, extracted)
    reloadWithAppended(state, settings)
  }
}
