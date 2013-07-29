package com.typesafe.sbtrc

import sbt.State
import com.typesafe.sbt.ui._
import com.typesafe.sbtrc.controller.DefaultsShim
import com.typesafe.sbtrc.controller.PlaySupport
import com.typesafe.sbtrc.io.ShimWriter

package object controller {

  type RequestHandler = (State, Context, Params) => (State, Params)

  def findHandler(name: String, state: State): Option[RequestHandler] = {
    val finder = EclipseSupport.findEclipseHandler orElse
      IdeaSupport.findIdeaHandler orElse {
        if (PlaySupport.isPlayProject(state))
          PlaySupport.findHandler
        else
          DefaultsShim.findHandler
      }

    if (finder.isDefinedAt(name))
      Some(finder(name))
    else
      None

    // TODO the above implementation is obviously kinda bogus.
    // So how do we implement findHandler? One approach could be that for any task key,
    // we have a way to look up a RequestHandler.
    // Maybe the convention is that we store the "UI handler" inside the scope
    // of the regular task, so "uiHandler in compile" gives you the "ui.RequestHandler" that goes
    // with the compile task?

    // OR, eventually, can we get taskStreams to have a ui context, the same way it has a logger?
    //  taskStreams <<= taskStreams apply { wrapTaskStream(_) }
    // implicit def withUi(t: TaskStream): { def ui: UIContext } = ...
    // That way, you just pull in one "contextual notifier thingy" and you can update both a UI and a console log.
    // (however this doesn't address how we go from input string to task inputs, and task result to output string)
  }

  // make adjustments to ShimWriter.knownShims based on State
  private val shimFilters = Map[String, State => Boolean]("play" -> PlaySupport.isPlayProject)

  // returns true if we need to reboot (any changes were made)
  def installShims(state: State): Boolean = {
    ShimWriter.knownShims.foldLeft(false) { (sofar, name) =>
      val installer = new ShimInstaller(name)
      val shouldInstall = shimFilters.get(name).getOrElse { state: State => true }
      if (shouldInstall(state))
        installer.ensure(state) || sofar // note, DO NOT short-circuit
      else
        sofar
    }
  }
}
