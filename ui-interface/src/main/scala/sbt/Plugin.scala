package sbt

import sbt.ScopeAxis.scopeAxisToScope
import play.api.libs.json._
import std.TaskStreams

/**
 * This UI plugin provides the basic settings used by plugins that want to be able to communicate with a UI.
 *
 * Basically, we just stub out the setting you can use to look up the current UI context.
 */
object SbtUiPlugin extends AutoPlugin {

  override def trigger = AllRequirements
  override def requires = plugins.CorePlugin

  // TODO why isn't this just globalSettings instead of putting everything in Global
  override val globalSettings: Seq[Setting[_]] = Seq(
    UIContext.uiContext <<= (UIContext.uiContext in Global) ?? CommandLineUiContext,
    UIContext.registeredFormats <<= (UIContext.registeredFormats in Global) ?? Nil,
    BackgroundJob.jobManager := { new CommandLineBackgroundJobManager() },
    Keys.onUnload := { s => try Keys.onUnload.value(s) finally BackgroundJob.jobManager.value.close() },
    BackgroundJob.jobList := { BackgroundJob.jobManager.value.list() })
  // TODO implement jobStop and jobWaitFor (requires writing a job ID parser)

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
}

// this default implementation is fine for both command line and UI, probably
private[sbt] abstract class AbstractBackgroundJobManager extends BackgroundJobManager {
  private final val nextId = new java.util.concurrent.atomic.AtomicLong(1)

  // this mutable state could conceptually go on State except
  // that then every task that runs a background job would have
  // to be a command, so not sure what to do here.
  @volatile
  private final var jobs = Set.empty[Handle]
  private def addJob(uiContext: UIContext, job: Handle): Unit = synchronized {
    // TODO send some kind of event on job add
    jobs += job
  }

  private def removeJob(uiContext: UIContext, job: Handle): Unit = synchronized {
    // TODO send some kind of event on job remove
    jobs -= job
  }

  private final class Handle(override val spawningTask: ScopedKey[_], val logger: Logger with java.io.Closeable,
    val uiContext: UIContext, val job: BackgroundJob) extends BackgroundJobHandle {

    override val id: Long = nextId.getAndIncrement()
    def humanReadableName: String = job.humanReadableName

    // EC for onStop handler below
    import concurrent.ExecutionContext.Implicits.global
    job.onStop { () =>
      logger.close()
      removeJob(uiContext, this)
    }

    addJob(uiContext, this)

    override final def equals(other: Any): Boolean = other match {
      case handle: BackgroundJobHandle if handle.id == id => true
      case _ => false
    }

    override final def hashCode(): Int = id.hashCode
  }

  protected def makeContext(streams: TaskStreams[ScopedKey[_]]): (Logger with java.io.Closeable, UIContext)

  override def runInBackground(streams: TaskStreams[ScopedKey[_]], start: (Logger, UIContext) => BackgroundJob): BackgroundJobHandle = {
    val (logger, uiContext) = makeContext(streams)
    val job = try new Handle(streams.key, logger, uiContext, start(logger, uiContext))
    catch {
      case e: Throwable =>
        logger.close()
        throw e
    }
    job
  }

  override def close(): Unit = {
    while (jobs.nonEmpty) {
      jobs.headOption.foreach { job =>
        job.job.shutdown()
        job.job.awaitTermination()
      }
    }
  }

  override def list(): Seq[BackgroundJobHandle] =
    jobs.toList

  private def withHandle[T](job: BackgroundJobHandle)(f: Handle => T): T = job match {
    case handle: Handle => f(handle)
    case other => sys.error(s"BackgroundJobHandle does not originate with the current BackgroundJobManager: $other")
  }

  override def stop(job: BackgroundJobHandle): Unit =
    withHandle(job)(_.job.shutdown())

  override def waitFor(job: BackgroundJobHandle): Unit =
    withHandle(job)(_.job.awaitTermination())

  override def toString(): String = s"BackgroundJobManager(jobs=${list().map(_.id).mkString})"
}

private[sbt] class CommandLineBackgroundJobManager extends AbstractBackgroundJobManager {
  override def makeContext(streams: TaskStreams[ScopedKey[_]]) = {
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
    (logger, CommandLineUiContext)
  }
}
