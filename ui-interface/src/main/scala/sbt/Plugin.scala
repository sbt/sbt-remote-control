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

private class BackgroundThreadPool extends java.io.Closeable {

  private val nextThreadId = new java.util.concurrent.atomic.AtomicInteger(1)
  private val threadGroup = Thread.currentThread.getThreadGroup()

  private val threadFactory = new java.util.concurrent.ThreadFactory() {
    override def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(threadGroup, runnable, s"sbt-bg-threads-${nextThreadId.getAndIncrement}")
      // Do NOT setDaemon because then the code in TaskExit.scala in sbt will insta-kill
      // the backgrounded process, at least for the case of the run task.
      thread
    }
  }

  private val executor = new java.util.concurrent.ThreadPoolExecutor(0, /* corePoolSize */
    32, /* maxPoolSize, max # of bg tasks */
    2, java.util.concurrent.TimeUnit.SECONDS, /* keep alive unused threads this long (if corePoolSize < maxPoolSize) */
    new java.util.concurrent.LinkedBlockingQueue[Runnable](),
    threadFactory)

  private class BackgroundRunnable(val taskName: String, body: () => Unit)
    extends Runnable with BackgroundJob {

    private val finishedLatch = new java.util.concurrent.CountDownLatch(1)

    private sealed trait Status
    private case object Waiting extends Status
    private final case class Running(thread: Thread) extends Status
    // the oldThread is None if we never ran
    private final case class Stopped(oldThread: Option[Thread]) extends Status

    // synchronize to read/write this, no sync to just read
    @volatile
    private var status: Status = Waiting

    // double-finally for extra paranoia that we will finishedLatch.countDown
    override def run() = try {
      val go = synchronized {
        status match {
          case Waiting =>
            status = Running(Thread.currentThread())
            true
          case Stopped(_) =>
            false
          case Running(_) =>
            throw new RuntimeException("Impossible status of bg thread")
        }
      }
      try { if (go) body() }
      finally cleanup()
    } finally finishedLatch.countDown()

    private class StopListener(val callback: () => Unit, val executionContext: concurrent.ExecutionContext) extends java.io.Closeable {
      override def close(): Unit = removeListener(this)
      override def hashCode: Int = System.identityHashCode(this)
      override def equals(other: Any): Boolean = other match {
        case r: AnyRef => this eq r
        case _ => false
      }
    }

    // access is synchronized
    private var stopListeners = Set.empty[StopListener]

    private def removeListener(listener: StopListener): Unit = synchronized {
      stopListeners -= listener
    }

    def cleanup(): Unit = {
      // avoid holding any lock while invoking callbacks, and
      // handle callbacks being added by other callbacks, just
      // to be all fancy.
      while (synchronized { stopListeners.nonEmpty }) {
        val listeners = synchronized {
          val list = stopListeners.toList
          stopListeners = Set.empty
          list
        }
        listeners.foreach { l =>
          l.executionContext.prepare().execute(new Runnable { override def run = l.callback() })
        }
      }
    }

    override def onStop(listener: () => Unit)(implicit ex: concurrent.ExecutionContext): java.io.Closeable = synchronized {
      val result = new StopListener(listener, ex)
      stopListeners += result
      result
    }

    override def awaitTermination(): Unit = finishedLatch.await()
    override def humanReadableName: String = taskName
    override def isRunning(): Boolean = status match {
      case Waiting => true // we start as running from BackgroundJob perspective
      case Running(thread) => thread.isAlive()
      case Stopped(threadOption) => threadOption.map(_.isAlive()).getOrElse(false)
    }
    override def shutdown(): Unit = synchronized {
      status match {
        case Waiting =>
          status = Stopped(None) // makes run() not run the body
        case Running(thread) =>
          status = Stopped(Some(thread))
          thread.interrupt()
        case Stopped(threadOption) =>
          // try to interrupt again! woot!
          threadOption.foreach(_.interrupt())
      }
    }
  }

  def run(manager: BackgroundJobManager, streams: std.TaskStreams[ScopedKey[_]])(work: (Logger, UIContext) => Unit): BackgroundJobHandle = {
    // TODO this gets me "compile:backgroundRun::streams" but I really want "compile:backgroundRun" -
    // not sure what the magic incantation is.
    val taskName = streams.key.scope.task.toOption.map(_.label).getOrElse("<unknown task>")
    def start(logger: Logger, uiContext: UIContext): BackgroundJob = {
      val runnable = new BackgroundRunnable(taskName, { () =>
        work(logger, uiContext)
      })

      executor.execute(runnable)

      runnable
    }

    manager.runInBackground(streams, start)
  }

  override def close(): Unit = {
    executor.shutdown()
  }
}

// this default implementation is fine for both command line and UI, probably
private[sbt] abstract class AbstractBackgroundJobManager extends BackgroundJobManager {
  private val nextId = new java.util.concurrent.atomic.AtomicLong(1)
  private val pool = new BackgroundThreadPool()

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

  override def runInBackgroundThread(streams: std.TaskStreams[ScopedKey[_]], start: (Logger, UIContext) => Unit): BackgroundJobHandle = {
    pool.run(this, streams)(start)
  }

  override def close(): Unit = {
    while (jobs.nonEmpty) {
      jobs.headOption.foreach { job =>
        job.job.shutdown()
        job.job.awaitTermination()
      }
    }
    pool.close()
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
