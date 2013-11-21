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
import com.typesafe.sbtrc.controller.PlaySupport
import scala.annotation.tailrec
import SbtCustomHacks._
import com.typesafe.sbtrc.ipc.JsonWriter

object SetupSbtChild extends AbstractServerCommand("0.12") {
  import SbtUtil._

  override def installPluginShims(s: State): Boolean = {
    controller.installShims(s)
  }

  override protected def handleRequest(s: State, context: ui.Context, request: protocol.Request): RequestResult = {
    try {
      // TODO - Clean this up for less indirection....
      controller.findHandler(request, s) map { handler =>
        val (newState, response) = handler(s, context, request)
        RequestSuccess(response, newState)
      } getOrElse RequestNotHandled
    } catch {
      case e: Exception => RequestFailure(e)
    }
  }
  // TODO - Is this correct?  maybe we should remove the previous UI context setting, or overwrite it.
  override protected def installRequestShims(serial: Long, context: ui.Context, state: State): State =
    reloadWithUiContext(state, context)

  override protected def installGlobalShims(state: State): State = {
    disableSelectMain(state)
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
}
