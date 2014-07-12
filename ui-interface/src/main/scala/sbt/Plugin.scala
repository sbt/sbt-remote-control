package sbt

import sbt.ScopeAxis.scopeAxisToScope
import play.api.libs.json._

/**
 * This UI plugin provides the basic settings used by plugins that want to be able to communicate with a UI.
 *
 * Basically, we just stub out the setting you can use to look up the current UI context.
 */
object SbtUiPlugin extends Plugin {

  override val buildSettings: Seq[Setting[_]] = Seq(
    UIContext.uiContext in Global <<= (UIContext.uiContext in Global) ?? CommandLineUiContext,
    UIContext.registeredFormats in Global <<= (UIContext.registeredFormats in Global) ?? Nil)

  def registerTaskSerialization[T](key: TaskKey[T])(implicit format: Format[T], mf: Manifest[T]): Setting[_] =
    UIContext.registeredFormats in Global += RegisteredFormat(format)(mf)
  def registerSettingSerialization[T](key: SettingKey[T])(implicit format: Format[T]): Setting[_] =
    UIContext.registeredFormats in Global += RegisteredFormat(format)(key.key.manifest)

}

private[sbt] object CommandLineUiContext extends AbstractUIContext {
  override def readLine(prompt: String, mask: Boolean): Option[String] = {
    val maskChar = if (mask) Some('*') else None
    SimpleReader.readLine(prompt, maskChar)
  }
  // TODO - Implement this better!    
  def confirm(msg: String): Boolean = {
    object Assent {
      def unapply(in: String): Boolean = {
        (in == "y" || in == "yes")
      }
    }
    SimpleReader.readLine(msg + " (yes/no): ", None) match {
      case Some(Assent()) => true
      case _ => false
    }
  }
  override def sendEvent[T: Writes](event: T): Unit = ()
  override def sendGenericEvent(data: JsValue): Unit = ()
  override def taskId: Long = 0L
}

