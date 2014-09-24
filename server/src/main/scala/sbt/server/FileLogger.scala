package sbt.server

import java.io._
import java.util.{ Timer, TimerTask }

/**
 * An interface of a "safe-ish" logger which can dump information to a file.
 *
 *  Implementations are expected to be safe for running inside the sbt-server,
 *  conforming to these expectations:
 *  - Will not overrun the disk
 *  - Will not use System.err/System.out, but if it does, will handle failures from
 *    doing so.
 *  - May throw exceptions when being created.
 *  - Keep # of open files to a minimum.
 *  - May write other files in the same directory as the one it owns.
 */
trait FileLogger {
  def log(msg: String): Unit
  def error(msg: String, e: Throwable): Unit
  def file: File
}
object FileLogger {
  def apply(f: File): SimpleRollingFileLogger = new SimpleRollingFileLogger(f)

  private[server] val flushTimer = new Timer(true) // isDaemon=true
}

/**
 * This is meant to be a rotating file log that can auto-squash its contents if they get too
 * big.  Right now we just dump a ton of information for debugging purposes.
 *
 * @param file  - The base file to write to.  Rolling files will show up with a similar name to this one.
 * @param maxFileSize - The (soft) maximum size we allow a file to grow before we roll
 * @param numFiles  - The number of files to keep, when rolling.
 */
class SimpleRollingFileLogger(
  val file: File,
  maxFileSize: Long = 5 * 1024 * 1024,
  numFiles: Long = 10) extends FileLogger {
  // Ensure the directory exists.
  sbt.IO.createDirectory(file.getParentFile)
  private var count = 1L
  private var written = 0L

  private val processName =
    try java.lang.management.ManagementFactory.getRuntimeMXBean().getName()
    catch {
      case _: Throwable => "(unknown)"
    }

  // Returns the next location to dump our current logs.
  private def dumpFile: File = synchronized {
    new File(file.getParentFile, f"${file.getName}.${count % numFiles}%02d")
  }

  private def openStream() = synchronized {
    val s = new PrintWriter(file, "UTF-8")
    s.println(s"New log file opened at ${new java.util.Date()} by ${processName}")
    s.flush() // be sure we never have a mysterious empty log file
    s
  }

  // make sure we can write to the file.
  file.getParentFile.mkdirs()
  private var stream = openStream()

  private def flush(): Unit = silentlyHandleSynchronized {
    stream.flush()
  }

  // If we don't do this, then the logs never flush until
  // there are more logs, which means the logs are rarely
  // up-to-date on disk until the process exits cleanly.
  private val flusher = new TimerTask() {
    override def run = flush()
  }

  // this can be pretty short, what's important is that when we get
  // a flood of debug logging or something we are batching up the
  // flush instead of doing it once per message.
  private final val timerIntervalMilliseconds = 1000 * 10
  FileLogger.flushTimer.schedule(flusher, timerIntervalMilliseconds, timerIntervalMilliseconds)

  def log(msg: String): Unit = silentlyHandleSynchronized {
    stream.write(msg)
    stream.write("\n")
    checkRotate(msg.length + 1)
  }

  def error(msg: String, e: Throwable): Unit = silentlyHandleSynchronized {
    stream.write(msg)
    stream.write("\n")
    e.printStackTrace(stream)
    // be extra-sure we get errors.
    stream.flush()
    // TODO - Stack traces are large...
    checkRotate(msg.length + 1)
  }

  /** Checks and rotates if we need to. */
  private def checkRotate(charsWritten: Long): Unit = silentlyHandleSynchronized {
    written += charsWritten
    if (written > maxFileSize) {
      val oldFile = dumpFile
      stream.println(s"Rolled to $oldFile at ${new java.util.Date()}")
      stream.flush()
      stream.close()
      // Note: This should be efficient as we're running in the same directory.
      // If a renameTo is unsucessfull, a slow move will occur.
      // We also allow rotating logs to fail, and we just reopen and continue
      // writing to the same file if we're unable to move the existing one.
      silentlyHandleSynchronized(sbt.IO.move(file, oldFile))
      // Bump the count of how many times we've rolled and reset.
      count += 1
      written = 0
      stream = openStream()
    }
  }

  def close(): Unit = silentlyHandleSynchronized {
    flusher.cancel()
    stream.println(s"Closing the ${processName} logs at ${new java.util.Date()}, goodbye.")
    stream.flush()
    stream.close()
  }

  final def silentlyHandleSynchronized[A](f: => A): Unit = synchronized {
    try f catch {
      case ex: IOException =>
        // Here we explicitly ignore issues with writing to the log files.
        // If we're unable to log, we're pretty much hosed in getting messages
        // out *because* System.{out,err} are actually hooked to feed out
        // the network socket and we are run in a hidden process.
        // SO, we try to limp along and see if information will get
        // spewed out to clients.
        ()
      case ex: NullPointerException =>
        // WORKAROUND for JDK6 bug.
        // If someone calls `write` on a closed resource, we get a null pointer exception.
        // This is thrown by the piece of code which OVERRIDES System.out/System.err, so it's
        // unsafe to do anything here.  We'll just limp along at this point.
        // This happens if, e.g. the server is asked to die/quit and some stdout/stderr is written
        // after that point.
        ()
    }
  }
}

