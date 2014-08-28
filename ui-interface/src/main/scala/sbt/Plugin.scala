package sbt

import sbt.ScopeAxis.scopeAxisToScope
import play.api.libs.json._
import std.TaskStreams

/**
 * This UI plugin provides the basic settings used by plugins that want to be able to communicate with a UI.
 *
 * Basically, we just stub out the setting you can use to look up the current UI context.
 */
object SbtUIPlugin extends AutoPlugin {

  override def trigger = AllRequirements
  override def requires = plugins.CorePlugin

  // TODO why isn't this just globalSettings instead of putting everything in Global
  override val globalSettings: Seq[Setting[_]] = Seq(
    UIKeys.interactionService in Global <<= (UIKeys.interactionService in Global) ?? CommandLineUIServices,
    UIKeys.sendEventService in Global <<= (UIKeys.sendEventService in Global) ?? CommandLineUIServices,
    UIKeys.registeredFormats in Global <<= (UIKeys.registeredFormats in Global) ?? Nil,
    BackgroundJob.jobManager := { new CommandLineBackgroundJobManager() },
    Keys.onUnload := { s => try Keys.onUnload.value(s) finally BackgroundJob.jobManager.value.close() },
    BackgroundJob.jobList := { BackgroundJob.jobManager.value.list() })
  // TODO implement jobStop and jobWaitFor (requires writing a job ID parser)

  def registerTaskSerialization[T](key: TaskKey[T])(implicit format: Format[T], mf: Manifest[T]): Setting[_] =
    UIKeys.registeredFormats in Global += RegisteredFormat(format)(mf)
  def registerSettingSerialization[T](key: SettingKey[T])(implicit format: Format[T]): Setting[_] =
    UIKeys.registeredFormats in Global += RegisteredFormat(format)(key.key.manifest)
}

private[sbt] object CommandLineUIServices extends AbstractInteractionService with AbstractSendEventService {
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
}

private[sbt] class CommandLineBackgroundJobManager extends AbstractBackgroundJobManager {
  override def makeContext(id: Long, streams: TaskStreams[ScopedKey[_]]) = {
    // TODO this is no good; what we need to do is replicate how sbt
    // gets loggers from Streams, but without the thing where they
    // are all closed when the Streams is closed. So we need "detached"
    // loggers. Potentially on command line we also want to avoid
    // showing the logs on the console as they arrive and only store
    // them in the file for retrieval with "last" - except "last"
    // takes a task name which we don't have.
    val logger = new Logger with java.io.Closeable {
      // TODO
      override def close(): Unit = ()
      // TODO
      override def log(level: sbt.Level.Value, message: => String): Unit = System.err.println(s"background log: $level: $message")
      // TODO
      override def success(message: => String): Unit = System.out.println(s"bg job success: $message")
      // TODO
      override def trace(t: => Throwable): Unit = t.printStackTrace(System.err)
    }
    (logger, CommandLineUIServices)
  }
}
