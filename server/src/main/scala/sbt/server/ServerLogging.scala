package sbt
package server

import scala.util.matching.Regex
import java.io.Writer
import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicReference
import sbt.protocol.CoreLogEvent
import sbt.protocol.TaskLogEvent
import play.api.libs.json.Writes
import scala.annotation.tailrec

// Our replacement for the global logger that allows you to swap out who is listening to events.
private[sbt] abstract class EventLogger[E <: protocol.LogEvent](protected val logSink: JsonSink[E]) extends BasicLogger {
  protected val peer: AtomicReference[Option[String => Unit]] = new AtomicReference(None)

  def updatePeer(f: String => Unit): Unit = peer.lazySet(Some(f))

  def send(entry: protocol.LogEntry): Unit

  def trace(t: => Throwable): Unit = {
    send(protocol.LogTrace(t.getClass.getSimpleName, t.getMessage))
  }

  def success(message: => String): Unit = {
    send(protocol.LogSuccess(message))
  }

  def log(level: Level.Value, message: => String): Unit = {
    send(protocol.LogMessage(level.toString, message))
  }

  def control(event: sbt.ControlEvent.Value, message: => String): Unit = {
    // TODO - mechanisms needed from AbstractLogger, implement these in some decent fashion instead of hackery.
    //        although, this method is actually deprecated.
    ()
  }
  // TODO - we should bundle these in one message...
  def logAll(events: Seq[sbt.LogEvent]): Unit = events foreach log

  private val ansiCodeRegex = "\\033\\[[0-9;]+m".r
  private val logLevelRegex = new Regex("^\\[([a-z]+)\\] *(.*)", "level", "message")

  private def removeAnsiCodes(line: String): String =
    ansiCodeRegex.replaceAllIn(line, "")

  private class LogWriter(lineToEntry: String => protocol.LogEntry) extends java.io.Writer {
    private def logLine(line: String): Unit = {
      send(lineToEntry(removeAnsiCodes(line)))
    }

    private val consoleBuf = new java.lang.StringBuilder()

    private def flushConsoleBuf(): Unit = {
      val maybeLine = consoleBuf.synchronized {
        val i = consoleBuf.indexOf("\n")
        if (i >= 0) {
          val line = consoleBuf.substring(0, i)
          consoleBuf.delete(0, i + 1)
          Some(line)
        } else {
          None
        }
      }

      for (line <- maybeLine) {
        logLine(line)
        flushConsoleBuf()
      }
    }

    override def write(chars: Array[Char], offset: Int, length: Int): Unit = {
      consoleBuf.synchronized {
        consoleBuf.append(chars, offset, length);
      }
    }

    override def flush(): Unit = flushConsoleBuf

    override def close(): Unit = flushConsoleBuf
  }

  private def lineToMessage(line: String): protocol.LogEntry = {
    logLevelRegex.findFirstMatchIn(line) flatMap { m =>
      val levelString = m.group("level")
      val message = m.group("message")
      Level(levelString) match {
        // These messages are duplicated by the logger itself.
        case Some(level) => Some(protocol.LogMessage(level.toString, "Read from stdout: " + message))
        case None => levelString match {
          case "success" => Some(protocol.LogSuccess(message))
          case _ => None
        }
      }
    } getOrElse {
      protocol.LogMessage(Level.Info.toString, line)
    }
  }
  private val consoleWriter = new LogWriter(lineToMessage)
  private[sbt] val consoleOut = ConsoleOut.printWriterOut(new PrintWriter(consoleWriter))
  private[sbt] val voidConsoleOut = ConsoleOut.printWriterOut(new PrintWriter(new java.io.Writer() {
    override def write(cbuf: Array[Char], off: Int, len: Int) = ()
    override def close(): Unit = ()
    override def flush(): Unit = ()
  }))

  // annoyingly, to go PrintStream to Writer we need to go via an OutputStream because
  // we need something for the PrintStream to wrap. Kind of ridiculous.
  // Also inefficient.
  private class LogStream(lineToEntry: String => protocol.LogEntry) extends java.io.OutputStream {
    import java.nio.charset._

    private val writer = new LogWriter(lineToEntry)
    private val decoder = new ByteDecoder()

    // we ALWAYS get UTF-8, see how we set up the print streams below in takeoverSystemStreams.
    // Also the print stream is set to autoFlush so on every newline we should flush
    override def write(byte: Int): Unit = synchronized {
      decoder.feed(byte.toByte)
    }
    override def flush(): Unit = synchronized {
      decoder.read() foreach { s =>
        writer.append(s)
      }
      writer.flush()
    }
    override def close(): Unit = synchronized {
      decoder.finish()
      flush()
      writer.close()
    }
  }

  def takeoverSystemStreams(): Unit = {
    def printStream(lineToEntry: String => protocol.LogEntry): java.io.PrintStream = {
      val logStream = new LogStream(lineToEntry)
      new java.io.PrintStream(logStream, true /* autoflush */ , "UTF-8")
    }
    System.setOut(printStream({ s => protocol.LogStdOut(s) }))
    System.setErr(printStream({ s => protocol.LogStdErr(s) }))
  }
}

private[sbt] class TaskEventLogger(taskIdFinder: TaskIdFinder, logSink: JsonSink[protocol.LogEvent])
  extends EventLogger[protocol.LogEvent](logSink) {
  override def send(entry: protocol.LogEntry): Unit = {
    // TODO we want to get the taskIfKnown by creating a streamsManager which generates
    // a custom stream for each task which records the task's key.
    // That will eliminate the need for heuristic BS based on which thread we are in.
    // But for now we don't have the taskIfKnown ever.
    val taskIdOption = taskIdFinder.bestGuessTaskIdOption(taskIfKnown = None)
    if (taskIdOption.isDefined)
      taskIdOption.foreach(taskId => logSink.send(TaskLogEvent(taskId, entry)))
    else
      logSink.send(CoreLogEvent(entry))
    peer.get match {
      case Some(f) => f(entry.message)
      case None => ()
    }
  }
}

private[sbt] class BackgroundJobEventLogger(jobId: Long, logSink: JsonSink[protocol.BackgroundJobLogEvent])
  extends EventLogger[protocol.BackgroundJobLogEvent](logSink) with java.io.Closeable {
  override def send(entry: protocol.LogEntry): Unit = {
    logSink.send(protocol.BackgroundJobLogEvent(jobId, entry))
    peer.get match {
      case Some(f) => f(entry.message)
      case None => ()
    }
  }
  override def close(): Unit = {}
}

// this is not synchronized (its user should synchronize as required)
private[server] class ByteDecoder(bufSize: Int = 256) {
  import java.nio.charset._
  import java.nio.ByteBuffer
  import java.nio.CharBuffer

  private val decoder = Charset.forName("UTF-8").newDecoder()
    .onMalformedInput(CodingErrorAction.REPLACE)
    .onUnmappableCharacter(CodingErrorAction.REPLACE)

  // The way Buffer works is that clear() makes it ready
  // to write and flip() makes it ready to read what was
  // written. Between methods, as invariants
  // "in" and "out" are always in write mode.
  private var in = ByteBuffer.allocate(bufSize)
  private var out = CharBuffer.allocate(bufSize)
  private var decoded: Vector[String] = Vector.empty
  private var finished = false

  private def clearOutBuffer(): Unit = {
    if (out.position > 0) {
      out.flip() // enter read mode
      decoded = decoded :+ out.toString
      // re-enter write mode
      out.clear()
    }
  }

  @tailrec
  private def decodeLoop(endOfInput: Boolean): Unit = {
    if (in.position > 0) {
      in.flip() // set up for reading

      val result = decoder.decode(in, out, endOfInput)

      in.compact() // this moves leftovers to the front and re-enters write mode

      result match {
        case CoderResult.UNDERFLOW =>
        // there may be bytes left in input but we can't process them yet
        case CoderResult.OVERFLOW =>
          // not enough space in the output buffer, so clear it out and
          // keep going...
          clearOutBuffer()
          decodeLoop(endOfInput)
      }
    }
  }

  def feed(byte: Byte): Unit = {
    require(!finished)
    if (in.hasRemaining) {
      in.put(byte)
    } else {
      in.flip() // make it a read buffer
      val larger = ByteBuffer.allocate(in.remaining + 256)
      larger.put(in)
      larger.put(byte)
      in = larger
    }
  }

  def feed(bytes: Array[Byte]): Unit = {
    // we don't copy the array, which depends on knowing
    // that the ByteBuffer variant below never keeps around
    // the passed-in ByteBuffer (it copies for us)
    feed(ByteBuffer.wrap(bytes, 0, bytes.length))
  }

  def feed(toAdd: ByteBuffer): Unit = {
    require(!finished)
    if (in.remaining >= toAdd.remaining) {
      in.put(toAdd)
    } else {
      in.flip() // make it a read buffer
      val combined = ByteBuffer.allocate(in.remaining + toAdd.remaining + 256)
      combined.put(in)
      combined.put(toAdd)
      in = combined
    }
  }

  def finish(): Unit = {
    if (!finished) {
      finished = true

      decodeLoop(endOfInput = true)

      clearOutBuffer()

      val result = decoder.flush(out)
      result match {
        case CoderResult.UNDERFLOW =>
        case CoderResult.OVERFLOW =>
          throw new Exception("should not be 256 chars created by flush")
      }

      clearOutBuffer()
    }
  }

  def read(): Seq[String] = {
    decodeLoop(endOfInput = finished)
    clearOutBuffer()

    val result = decoded
    decoded = Vector.empty
    result
  }
}
