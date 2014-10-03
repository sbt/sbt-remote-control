package sbt
package server

import BuildStreams.Streams
import java.io.BufferedReader
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.PrintWriter

class LoggerWithTaskId(val taskId: Long, underlying: AbstractLogger) extends Logger {

  def trace(t: => Throwable): Unit = underlying.trace(t)

  def success(message: => String): Unit = underlying.success(message)

  def log(level: Level.Value, message: => String): Unit = underlying.log(level, message)

  def control(event: sbt.ControlEvent.Value, message: => String): Unit = underlying.control(event, message)
}

class ServerTaskStreams(underlying: std.ManagedStreams[ScopedKey[_]]) extends UnsealedManagedStreams[ScopedKey[_]] {
  override def key: ScopedKey[_] =
    underlying.key

  override def readText(key: ScopedKey[_], sid: String): BufferedReader =
    underlying.readText(key, sid)

  override def readBinary(key: ScopedKey[_], sid: String): BufferedInputStream =
    underlying.readBinary(key, sid)

  override def text(sid: String): PrintWriter =
    underlying.text(sid)

  override def binary(sid: String): BufferedOutputStream =
    underlying.binary(sid)

  override def cacheDirectory: File =
    underlying.cacheDirectory

  override def log(sid: String): Logger =
    underlying.log(sid)

  override def open(): Unit = underlying.open()
  override def close(): Unit = underlying.close()
  override def isClosed: Boolean = underlying.isClosed
}

// This is the "streams manager" which is a factory for per-task streams
class ServerStreams(underlying: std.Streams[ScopedKey[_]]) extends std.Streams[ScopedKey[_]] {
  override def apply(key: ScopedKey[_]): std.ManagedStreams[ScopedKey[_]] = {
    val orig = underlying.apply(key)
    new ServerTaskStreams(orig)
  }
}

object ServerStreams {
  def replaceStreams(state: State): State = {
    state.get(Keys.stateStreams) match {
      case Some(old: ServerStreams) =>
        state
      case Some(old: Streams) =>
        old match {
          case c: java.io.Closeable => c.close()
          case _ =>
        }
        state.put(Keys.stateStreams, new ServerStreams(old))
      case _ =>
        val extracted = Project.extract(state)
        val old = BuildStreams.mkStreams(extracted.structure.units, extracted.structure.root, extracted.structure.data)(state)
        state.put(Keys.stateStreams, new ServerStreams(old))
    }
  }
}
