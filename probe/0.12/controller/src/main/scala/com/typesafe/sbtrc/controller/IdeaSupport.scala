package com.typesafe.sbtrc
package controller

import sbt._
import com.typesafe.sbt.ui.{ Context => UIContext, Params }
import com.typesafe.sbtrc.protocol

object IdeaSupport {
  val findIdeaHandler: PartialFunction[String, RequestHandler] = {
    case "gen-idea" => generateProject
  }

  private val ideaShimGenerateTask = InputKey[Unit]("idea-shim-generate")

  private def generateProject(state: State, context: UIContext, params: Params): (State, Params) = {
    val s = SbtUtil.runInputTask(ideaShimGenerateTask, state, args = " no-sbt-build-module", context = Some(context))
    (s, Params("application/json", "{}"))
  }
}
