package sbt

import scala.util.control.NonFatal
import std.TaskStreams

/**
 * Interface between sbt and a thing running in the background.
 */
private[sbt] trait BackgroundJob {
  def humanReadableName: String
  // TODO return success/fail?
  def awaitTermination(): Unit
  def shutdown(): Unit
  // this should be true on construction and stay true until
  // the job is complete
  def isRunning(): Boolean
  // called after stop or on spontaneous exit, closing the result
  // removes the listener
  def onStop(listener: () => Unit)(implicit ex: concurrent.ExecutionContext): java.io.Closeable
  // do we need this or is the spawning task good enough?
  // def tags: SomeType
}

/**
 * Interface between tasks and jobs; tasks aren't allowed
 *  to directly mess with the BackgroundJob above. Methods
 *  on this interface should all be pure (conceptually this
 *  is immutable).
 */
trait BackgroundJobHandle {
  def id: Long
  def humanReadableName: String
  def spawningTask: ScopedKey[_]
  // def tags: SomeType

  override def toString = s"BackgroundJobHandle(${id},${humanReadableName},${Def.showFullKey(spawningTask)})"
}

trait BackgroundJobManager extends java.io.Closeable {

  /**
   * Launch a background job which is a function that runs inside another thread;
   *  killing the job will interrupt() the thread. If your thread blocks on a process,
   *  then you should get an InterruptedException while blocking on the process, and
   *  then you could process.destroy() for example.
   */
  def runInBackgroundThread(streams: std.TaskStreams[ScopedKey[_]], start: (Logger, SendEventService) => Unit): BackgroundJobHandle

  def list(): Seq[BackgroundJobHandle]
  def stop(job: BackgroundJobHandle): Unit
  def waitFor(job: BackgroundJobHandle): Unit
  final def stop(id: Long): Unit =
    list().find(_.id == id).foreach(stop(_))
  final def waitFor(id: Long): Unit =
    list().find(_.id == id).foreach(waitFor(_))
}

object BackgroundJob {
  // this is a setting not a task because semantically it's required to always be the same one
  val jobManager = settingKey[BackgroundJobManager]("Job manager used to run background jobs.")
  val jobList = taskKey[Seq[BackgroundJobHandle]]("List running background jobs.")
  val jobStop = inputKey[Unit]("Stop a background job by providing its ID.")
  val jobWaitFor = inputKey[Unit]("Wait for a background job to finish by providing its ID.")
  val backgroundRun = inputKey[BackgroundJobHandle]("Start an application's default main class as a background job")
  val backgroundRunMain = inputKey[BackgroundJobHandle]("Start a provided main class as a background job")
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

  def run(manager: AbstractBackgroundJobManager, streams: std.TaskStreams[ScopedKey[_]])(work: (Logger, SendEventService) => Unit): BackgroundJobHandle = {
    // TODO this gets me "compile:backgroundRun::streams" but I really want "compile:backgroundRun" -
    // not sure what the magic incantation is.
    val taskName = streams.key.scope.task.toOption.map(_.label).getOrElse("<unknown task>")
    def start(logger: Logger, uiContext: SendEventService): BackgroundJob = {
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

// Shared by command line and UI implementation
private[sbt] abstract class AbstractBackgroundJobManager extends BackgroundJobManager {
  private val nextId = new java.util.concurrent.atomic.AtomicLong(1)
  private val pool = new BackgroundThreadPool()

  // hooks for sending start/stop events
  protected def onAddJob(uiContext: SendEventService, job: BackgroundJobHandle): Unit = {}
  protected def onRemoveJob(uiContext: SendEventService, job: BackgroundJobHandle): Unit = {}

  // this mutable state could conceptually go on State except
  // that then every task that runs a background job would have
  // to be a command, so not sure what to do here.
  @volatile
  private final var jobs = Set.empty[Handle]
  private def addJob(uiContext: SendEventService, job: Handle): Unit = synchronized {
    onAddJob(uiContext, job)
    jobs += job
  }

  private def removeJob(uiContext: SendEventService, job: Handle): Unit = synchronized {
    onRemoveJob(uiContext, job)
    jobs -= job
  }

  private final class Handle(override val id: Long, override val spawningTask: ScopedKey[_],
    val logger: Logger with java.io.Closeable, val uiContext: SendEventService, val job: BackgroundJob)
    extends BackgroundJobHandle {

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

  protected def makeContext(id: Long, streams: TaskStreams[ScopedKey[_]]): (Logger with java.io.Closeable, SendEventService)

  def runInBackground(streams: TaskStreams[ScopedKey[_]], start: (Logger, SendEventService) => BackgroundJob): BackgroundJobHandle = {
    val id = nextId.getAndIncrement()
    val (logger, uiContext) = makeContext(id, streams)
    val job = try new Handle(id, streams.key, logger, uiContext, start(logger, uiContext))
    catch {
      case e: Throwable =>
        logger.close()
        throw e
    }
    job
  }

  override def runInBackgroundThread(streams: std.TaskStreams[ScopedKey[_]], start: (Logger, SendEventService) => Unit): BackgroundJobHandle = {
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
