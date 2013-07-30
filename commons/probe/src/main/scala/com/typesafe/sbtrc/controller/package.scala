package com.typesafe.sbtrc

import sbt._
import com.typesafe.sbt.ui._
import com.typesafe.sbtrc.io.ShimWriter


package object controller {

  type RequestHandler = (State, Context, Params) => (State, Params)


  // TODO - Specify project too...
  def isPlayProject(state: State): Boolean = {
    val extracted = Project.extract(state)
    extracted.getOpt(SettingKey[Boolean]("play-plugin")).isDefined
  }

  // make adjustments to ShimWriter.knownShims based on State
  private val shimFilters = Map[String, State => Boolean]("play" -> isPlayProject)

  // returns true if we need to reboot (any changes were made)
  // TODO - These versions and passing are all out of whack.
  def installShims(state: State, sbtVersion: String = "0.12"): Boolean = {
    ShimWriter.knownShims.foldLeft(false) { (sofar, name) =>
      val installer = new ShimInstaller(name, sbtVersion)
      val shouldInstall = shimFilters.get(name).getOrElse { state: State => true }
      if (shouldInstall(state))
        installer.ensure(state) || sofar // note, DO NOT short-circuit
      else
        sofar
    }
  }
}
