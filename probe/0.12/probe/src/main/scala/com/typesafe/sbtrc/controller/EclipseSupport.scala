package com.typesafe.sbtrc
package controller

import sbt._
import com.typesafe.sbt.ui.{ Context => UIContext, Params }
import com.typesafe.sbtrc.protocol

object EclipseSupport {
  val findEclipseHandler: PartialFunction[String, RequestHandler] = {
    case "eclipse" => generateProject
  }

  private val eclipseShimGenerateTask = InputKey[Unit]("eclipse-shim-generate")

  private def generateProject(state: State, context: UIContext, params: Params): (State, Params) = {
    val s = SbtUtil.runInputTask(eclipseShimGenerateTask, state, args = "", context = Some(context))
    (s, Params("application/json", "{}"))
  }
}
