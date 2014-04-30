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
  /**
   * Sends an evnet out to all registered listenres.
   * 
   * Note:  Requires the appropriate format to be registered
   *        via `registeredFormats` key.
   */
  def sendRawEvent[T: Manifest](event: T): Unit
  def sendGenericEvent(data: JsValue): Unit

  // obtain the task ID that should be included in events
  def taskId: Long
}
/** Represents a Manifest/Format pair we can use
 *  to serialize task values + events later.
 */
sealed trait RegisterFormat {
  type T
  def manifest: Manifest[T]
  def format: Format[T]
}
object RegisterFormat {
  def apply[U](f: Format[U])(implicit mf: Manifest[U]):  RegisterFormat =
    new RegisterFormat {
       type T = U
       override val format = f
       override val manifest = mf
    }
}
object UIContext {
  val uiContext = TaskKey[UIContext]("ui-context", "The context used to communicate to a user interface running sbt.")
  val registeredFormats = settingKey[Seq[RegisterFormat]]("All the formats needed to serialize events/messages to the client.")
}
private[sbt] abstract class AbstractUIContext extends UIContext