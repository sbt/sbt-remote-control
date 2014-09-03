package sbt

import complete.Parser
import complete.DefaultParsers
import Keys._
import Def.Initialize
import sbinary.DefaultProtocol.StringFormat
import Cache.seqFormat
import Attributed.data

object SbtBackgroundRunPlugin extends AutoPlugin {
  override def trigger = AllRequirements
  override def requires = plugins.JvmPlugin

  override val globalSettings: Seq[Setting[_]] = Seq(
    BackgroundJob.jobManager := { new CommandLineBackgroundJobManager() },
    Keys.onUnload := { s => try Keys.onUnload.value(s) finally BackgroundJob.jobManager.value.close() },
    BackgroundJob.jobList := { BackgroundJob.jobManager.value.list() },
    BackgroundJob.jobStop <<= jobStopTask(),
    BackgroundJob.jobWaitFor <<= jobWaitForTask())

  override val projectSettings = inConfig(Compile)(Seq(
    BackgroundJob.backgroundRunMain <<= backgroundRunMainTask(fullClasspath, runner in run),
    BackgroundJob.backgroundRun <<= backgroundRunTask(fullClasspath, mainClass in run, runner in run)))

  private def backgroundRunMainTask(classpath: Initialize[Task[Classpath]], scalaRun: Initialize[Task[ScalaRun]]): Initialize[InputTask[BackgroundJobHandle]] =
    {
      import DefaultParsers._
      val parser = Defaults.loadForParser(discoveredMainClasses)((s, names) => Defaults.runMainParser(s, names getOrElse Nil))
      Def.inputTask {
        val (mainClass, args) = parser.parsed
        BackgroundJob.jobManager.value.runInBackgroundThread(Keys.resolvedScoped.value, { (logger, uiContext) =>
          toError(scalaRun.value.run(mainClass, data(classpath.value), args, logger))
        })
      }
    }

  private def backgroundRunTask(classpath: Initialize[Task[Classpath]], mainClassTask: Initialize[Task[Option[String]]], scalaRun: Initialize[Task[ScalaRun]]): Initialize[InputTask[BackgroundJobHandle]] =
    {
      import Def.parserToInput
      import sys.error

      val parser = Def.spaceDelimited()
      Def.inputTask {
        val mainClass = mainClassTask.value getOrElse error("No main class detected.")
        BackgroundJob.jobManager.value.runInBackgroundThread(Keys.resolvedScoped.value, { (logger, uiContext) =>
          toError(scalaRun.value.run(mainClass, data(classpath.value), parser.parsed, logger))
        })
      }
    }

  private def jobIdParser: (State, Seq[BackgroundJobHandle]) => Parser[Seq[BackgroundJobHandle]] = {
    import DefaultParsers._
    (state, handles) => {
      val stringIdParser: Parser[Seq[String]] = Space ~> token(NotSpace examples handles.map(_.id.toString).toSet, description = "<job id>").+
      stringIdParser.map { strings =>
        strings.map(Integer.parseInt(_)).flatMap(id => handles.find(_.id == id))
      }
    }
  }

  // these three are copy-pasted from Defaults.scala to change them from
  // a static sbinary.Format to one generated from State
  private def loadFromContext[T](task: TaskKey[T], context: ScopedKey[_], s: State, fmt: State => sbinary.Format[T]): Option[T] =
    SessionVar.load(SessionVar.resolveContext(task.scopedKey, context.scope, s), s)(fmt(s))
  private def loadForParser[P, T](task: TaskKey[T], fmt: State => sbinary.Format[T])(f: (State, Option[T]) => Parser[P]): Initialize[State => Parser[P]] =
    loadForParserI(task, fmt)(Def value f)
  private def loadForParserI[P, T](task: TaskKey[T], fmt: State => sbinary.Format[T])(init: Initialize[(State, Option[T]) => Parser[P]]): Initialize[State => Parser[P]] =
    (resolvedScoped, init)((ctx, f) => (s: State) => f(s, loadFromContext(task, ctx, s, fmt)))

  private def foreachJobTask(f: (BackgroundJobManager, BackgroundJobHandle) => Unit): Initialize[InputTask[Unit]] = {
    import DefaultParsers._
    val formatGetter: State => sbinary.Format[Seq[BackgroundJobHandle]] = {
      s: State => seqFormat(Project.extract(s).get(BackgroundJob.jobManager).handleFormat)
    }
    val parser =
      loadForParser(BackgroundJob.jobList, formatGetter)((s, handles) => jobIdParser(s, handles getOrElse Nil))
    Def.inputTask {
      val handles = parser.parsed
      for (handle <- handles) {
        f(BackgroundJob.jobManager.value, handle)
      }
    }
  }

  private def jobStopTask(): Initialize[InputTask[Unit]] =
    foreachJobTask { (manager, handle) => manager.stop(handle) }

  private def jobWaitForTask(): Initialize[InputTask[Unit]] =
    foreachJobTask { (manager, handle) => manager.waitFor(handle) }
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

  def run(manager: BaseBackgroundJobManager, spawningTask: ScopedKey[_])(work: (Logger, SendEventService) => Unit): BackgroundJobHandle = {
    def start(logger: Logger, uiContext: SendEventService): BackgroundJob = {
      val runnable = new BackgroundRunnable(spawningTask.key.label, { () =>
        work(logger, uiContext)
      })

      executor.execute(runnable)

      runnable
    }

    manager.runInBackground(spawningTask, start)
  }

  override def close(): Unit = {
    executor.shutdown()
  }
}

// Shared by command line and UI implementation
private[sbt] abstract class BaseBackgroundJobManager extends AbstractBackgroundJobManager {
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

  // we use this if we deserialize a handle for a job that no longer exists
  private final class DeadHandle(override val id: Long, override val humanReadableName: String)
    extends BackgroundJobHandle {
    override val spawningTask: ScopedKey[_] = Keys.streams // just a dummy value
  }

  protected def makeContext(id: Long, spawningTask: ScopedKey[_]): (Logger with java.io.Closeable, SendEventService)

  def runInBackground(spawningTask: ScopedKey[_], start: (Logger, SendEventService) => BackgroundJob): BackgroundJobHandle = {
    val id = nextId.getAndIncrement()
    val (logger, uiContext) = makeContext(id, spawningTask)
    val job = try new Handle(id, spawningTask, logger, uiContext, start(logger, uiContext))
    catch {
      case e: Throwable =>
        logger.close()
        throw e
    }
    job
  }

  override def runInBackgroundThread(spawningTask: ScopedKey[_], start: (Logger, SendEventService) => Unit): BackgroundJobHandle = {
    pool.run(this, spawningTask)(start)
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

  private def withHandle(job: BackgroundJobHandle)(f: Handle => Unit): Unit = job match {
    case handle: Handle => f(handle)
    case dead: DeadHandle => () // nothing to stop or wait for
    case other => sys.error(s"BackgroundJobHandle does not originate with the current BackgroundJobManager: $other")
  }

  override def stop(job: BackgroundJobHandle): Unit =
    withHandle(job)(_.job.shutdown())

  override def waitFor(job: BackgroundJobHandle): Unit =
    withHandle(job)(_.job.awaitTermination())

  override def toString(): String = s"BackgroundJobManager(jobs=${list().map(_.id).mkString})"

  override val handleFormat: sbinary.Format[BackgroundJobHandle] = {
    import sbinary.DefaultProtocol._
    wrap[BackgroundJobHandle, (Long, String)](h => (h.id, h.humanReadableName),
      {
        case (id, humanReadableName) =>
          // resurrect the actual handle, or use a dead placeholder
          jobs.find(_.id == id).getOrElse(new DeadHandle(id, humanReadableName + " <job no longer exists>"))
      })
  }
}

private[sbt] class CommandLineBackgroundJobManager extends BaseBackgroundJobManager {
  override def makeContext(id: Long, spawningTask: ScopedKey[_]) = {
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
