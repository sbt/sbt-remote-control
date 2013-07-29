package com.typesafe.sbtrc
package controller

import sbt._
import com.typesafe.sbtrc.protocol.TaskNames
import com.typesafe.sbt.ui.{ Context => UIContext, Params }
import com.typesafe.sbt.ui.{ Context => UIContext }
import com.typesafe.sbtrc.controller.DefaultsShim.{
  compileHandler,
  discoveredMainClassesHandler,
  makeResponseParams,
  nameHandler,
  runMainHandler,
  testHandler,
  watchTransitiveSourcesHandler
}
import sbt.Scoped.inputScopedToKey

/** This guy provides the support we need to run play projects... */
object PlaySupport {

  private def playRunHandler(taskName: String): RequestHandler = { (state: State, context: UIContext, params: Params) =>
    run(taskName, state, context, params)
  }

  private val findPlayHandler: PartialFunction[String, RequestHandler] = {
    case TaskNames.run => playRunHandler(TaskNames.run)
    case TaskNames.runMain => playRunHandler(TaskNames.runMain)
  }

  val findHandler: PartialFunction[String, RequestHandler] =
    findPlayHandler orElse DefaultsShim.findHandler

  // This is the shim'd run task we use instead of play's default.
  private val playRunShimTask = InputKey[Unit]("play-shim-run")

  def run(taskName: String, state: State, context: UIContext, params: Params): (State, Params) = {
    // TODO - Lookup default port and ensure it's ready/running....
    val s = SbtUtil.runInputTask(playRunShimTask, state, args = "", context = Some(context))
    (s, makeResponseParams(protocol.RunResponse(success = true,
      task = taskName)))
  }

  // TODO - Specify project too...
  def isPlayProject(state: State): Boolean = {
    val extracted = Project.extract(state)
    extracted.getOpt(SettingKey[Boolean]("play-plugin")).isDefined
  }
}
