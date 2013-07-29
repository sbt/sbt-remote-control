package com.typesafe.sbt.ui

import _root_.sbt._
/**
 * This UI plugin provides the basic settings used by plugins that want to be able to communicate with a UI.
 *
 * Basically, we just stub out the setting you can use to look up the current UI context.
 */
object SbtUiPlugin extends Plugin {

  val uiContext = SettingKey[Context]("sbt-ui-context", "The context used to communicate to a user interface running sbt.")

  override val buildSettings: Seq[Setting[_]] = Seq(uiContext in Global <<= (uiContext in Global) ?? Context.noop)
}
