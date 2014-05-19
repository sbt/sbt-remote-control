package com.typesafe.sbtrc
package server

import java.io._

/** An interface of a "safe-ish" logger which can dump information to a file.
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
}
object FileLogger {
  def apply(f: File): FileLogger = new SimpleRollingFileLogger(f)
}

/**
 * This is meant to be a rotating file log that can auto-squash its contents if they get too
 * big.  Right now we just dump a ton of information for debugging purposes.
 *
 *
 * Note: This is SINGLE THREADED access, so no cheating.
 *
 * @param file  - The base file to write to.  Rolling files will show up with a similar name to this one.
 * @param maxFileSize - The (soft) maximum size we allow a file to grow before we roll
 * @param numFiles  - The number of files to keep, when rolling.
 */
class SimpleRollingFileLogger(file: File, maxFileSize: Long = 5 * 1024 * 1024, numFiles: Long = 10) extends FileLogger {
  // Ensure the directory exists.
  sbt.IO.createDirectory(file.getParentFile)
  private var count = 1L
  private var written = 0L
  // Returns the next location to dump our current logs.
  private def dumpFile: File =
    new File(file.getParentFile, f"${file.getName}.${count % numFiles}%02d")

  private def openStream() = new PrintWriter(new FileWriter(file))

  // make sure we can write to the file.
  file.getParentFile.mkdirs()
  private var stream = openStream()

  def log(msg: String): Unit = silentlyHandle {
    stream.write(msg)
    stream.write("\n")
    checkRotate(msg.length + 1)
  }

  def error(msg: String, e: Throwable): Unit = silentlyHandle {
    stream.write(msg)
    stream.write("\n")
    e.printStackTrace(stream)
    // TODO - Stack traces are large...
    checkRotate(msg.length + 1)
  }

  /** Checks and rotates if we need to. */
  private def checkRotate(charsWritten: Long): Unit = silentlyHandle {
    written += charsWritten
    if (written > maxFileSize) {
      stream.flush()
      stream.close()
      // Note: This should be efficient as we're running in the same directory.
      // If a renameTo is unsucessfull, a slow move will occur.
      // We also allow rotating logs to fail, and we just reopen and continue
      // writing to the same file if we're unable to move the existing one.
      silentlyHandle(sbt.IO.move(file, dumpFile))
      // Bump the count of how many times we've rolled and reset.
      count += 1
      written = 0
      stream = openStream()
    }
  }

  def close(): Unit = silentlyHandle(stream.close())

  final def silentlyHandle[A](f: => A): Unit =
    try f catch {
      case ex: IOException =>
        // Here we explicitly ignore issues with writing to the log files.
        // If we're unable to log, we're pretty much hosed in getting messages
        // out *because* System.{out,err} are actually hooked to feed out
        // the network socket and we are run in a hidden process.
        // SO, we try to limp along and see if information will get
        // spewed out to clients.
        ()
    }
}

