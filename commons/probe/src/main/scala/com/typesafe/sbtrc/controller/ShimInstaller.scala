package com.typesafe.sbtrc
package controller

import sbt._
import properties.SbtRcProperties._

class ShimInstaller(writer: io.ShimWriter) {

  // true if the shim was freshly installed
  def ensure(state: State): Boolean = {
    val extracted = Project.extract(state)
    val bd = extracted get (Keys.baseDirectory in ThisBuild)
    val installed = extracted.getOpt(SettingKey[Boolean](writer.name + "-shim-installed")) getOrElse false
    if (!installed) {
      if (writer.ensureExists(bd)) {
        System.err.println("Installing shim for " + writer.name + ", need to reboot SBT.")
        true
      } else {
        // this would mean that the foo-shim-installed setting is missing but the
        // .sbt file exists; which is probably just a bad situation; but we'll
        // attempt to proceed.
        System.err.println("shim for " + writer.name + " appears to have a config file but isn't in the project settings")
        false
      }
    } else {
      false
    }
  }
}
