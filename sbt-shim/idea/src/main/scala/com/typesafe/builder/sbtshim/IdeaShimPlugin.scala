package com.typesafe.builder.sbtshim

import sbt._
import Keys._
import com.typesafe.sbt.ui
import com.typesafe.sbt.ui.{ Context => UIContext }

object IdeaShimKeys {
  val ideaShimInstalled = SettingKey[Boolean]("idea-shim-installed")

  val ideaShimGenerate = InputKey[Unit]("idea-shim-generate")

  val uiContext = com.typesafe.sbt.ui.SbtUiPlugin.uiContext
}

object IdeaShimPlugin extends Plugin {
  import IdeaShimKeys._

  override val settings: Seq[Setting[_]] = Seq(
    ideaShimInstalled := true,
    // creating an input task that runs a command is officially not good
    ideaShimGenerate <<= inputTask { (args: TaskKey[Seq[String]]) =>
      (args, state, uiContext in Global) map generate
    })

  private def generate(args: Seq[String], state: State, ctx: UIContext): State = {
    Command.process("gen-idea" + (if (args.nonEmpty) args.mkString(" ", " ", "") else ""), state)
  }
}
