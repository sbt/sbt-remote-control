package com.typesafe.sbtrc

import sbt._
import com.typesafe.sbt.ui._
import com.typesafe.sbtrc.io.ShimWriter
import com.typesafe.sbtrc.properties.SbtRcProperties


package object controller {

  type RequestHandler = (State, Context, protocol.Request) => (State, protocol.Response)


  // TODO - Specify project too...
  def isPlayProject(state: State): Boolean = {
    val extracted = Project.extract(state)
    extracted.getOpt(SettingKey[Boolean]("play-plugin")).isDefined
  }

  // make adjustments to ShimWriter.knownShims based on State
  private val shimFilters = Map[String, State => Boolean]("play" -> isPlayProject)

  // returns true if we need to reboot (any changes were made)
  // TODO - These versions and passing are all out of whack.
  def installShims(state: State, sbtVersion: String = "0.12", extraShims: Seq[ShimWriter] = Seq.empty): Boolean = {
    val allShims = 
      ShimWriter.knownShims(SbtRcProperties.APP_VERSION, sbtVersion) ++ extraShims
    installRawShims(state, allShims)
  }
  // returns true if we need to reboot (any changes were made)
  // TODO - These versions and passing are all out of whack.
  def installRawShims(state: State, shims: Seq[ShimWriter] = Seq.empty): Boolean = {
    shims.foldLeft(false) { (sofar, writer) =>
      // TODO - We need to customize which shim installer is used based on sbt version!
      val installer = new ShimInstaller(writer)
      val shouldInstall = shimFilters.get(writer.name).getOrElse { state: State => true }
      if (shouldInstall(state))
        installer.ensure(state) || sofar // note, DO NOT short-circuit
      else
        sofar
    }
  }  
}
