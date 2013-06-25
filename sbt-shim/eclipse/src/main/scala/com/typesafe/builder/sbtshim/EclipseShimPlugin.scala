package com.typesafe.builder.sbtshim

import sbt._
import Keys._
import com.typesafe.sbt.ui
import com.typesafe.sbt.ui.{ Context => UIContext }
import com.typesafe.sbteclipse.core.Eclipse

object EclipseShimKeys {
  val eclipseShimInstalled = SettingKey[Boolean]("eclipse-shim-installed")

  val eclipseShimGenerate = InputKey[Unit]("eclipse-shim-generate")

  val uiContext = com.typesafe.sbt.ui.SbtUiPlugin.uiContext
}

object EclipseShimPlugin extends Plugin {
  import EclipseShimKeys._

  override val settings: Seq[Setting[_]] = Seq(
    eclipseShimInstalled := true,
    // creating an input task that runs a command is officially not good
    eclipseShimGenerate <<= inputTask { (args: TaskKey[Seq[String]]) =>
      (args, state, uiContext in Global) map generate
    })

  private def generate(args: Seq[String], state: State, ctx: UIContext): State = {
    Command.process("eclipse", state)
  }
}
