package com.typesafe.sbtrc
package server

import java.io._

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
 *
 * TODO - Rotate these logs...
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

  def log(msg: String): Unit = {
    stream.write(msg)
    stream.write("\n")
    stream.flush()
    checkRotate(msg.length + 1)
  }

  def error(msg: String, e: Throwable): Unit = {
    stream.write(msg)
    stream.write("\n")
    e.printStackTrace(stream)
    stream.flush()
    // TODO - Stack traces are large...
    checkRotate(msg.length + 1)
  }

  /** Checks and rotates if we need to. */
  private def checkRotate(charsWritten: Long): Unit = {
    written += charsWritten
    if (written > maxFileSize) {
      stream.close()
      // Note: This should be efficient as we're running in the same directory.
      file.renameTo(dumpFile)
      // Bump the count of how many times we've rolled and reset.
      count += 1
      written = 0
      stream = openStream()
    }
  }

  def close(): Unit = stream.close()
}

