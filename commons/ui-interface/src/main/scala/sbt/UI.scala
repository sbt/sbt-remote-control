package sbt

import play.api.libs.json._

sealed trait UIContext {
  /** Prompts the user for input, optionally with a mask for characters. */
  def readLine(prompt: String, mask: Boolean): Option[String]
  /** Ask the user to confirm something (yes or no) before continuing. */
  def confirm(msg: String): Boolean

  // TODO - Ask for input with autocomplete?

  /** Sends an event out to all registered event listeners. */
  def sendEvent[T: Format](event: T): Unit
  def sendGenericEvent(data: JsValue): Unit
}
object UIContext {
  val uiContext = SettingKey[UIContext]("ui-context", "The context used to communicate to a user interface running sbt.")
}
private[sbt] abstract class AbstractUIContext extends UIContext