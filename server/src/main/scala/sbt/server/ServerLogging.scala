package sbt
package server

import scala.util.matching.Regex
import java.io.Writer
import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicReference

// Our replacement for the global logger that allows you to swap out who is listening to events.
private[sbt] class EventLogger extends BasicLogger {
  private val client: AtomicReference[SbtClient] = new AtomicReference(NullSbtClient)
  private val peer: AtomicReference[Option[String => Unit]] = new AtomicReference(None)

  def updateClient(next: SbtClient): Unit = client.lazySet(next)
  def updatePeer(f: String => Unit): Unit = peer.lazySet(Some(f))

  def send(entry: protocol.LogEntry): Unit = {
    client.get.send(protocol.LogEvent(entry))
    peer.get match {
      case Some(f) => f(entry.message)
      case None => ()
    }
  }

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

  private def logLine(line: String): Unit = {
    val noCodes = ansiCodeRegex.replaceAllIn(line, "")
    logLineNoCodes(noCodes)
  }

  // log a "cooked" line (that already has [info] prepended etc.)
  private def logLineNoCodes(line: String): Unit = {
    val entry: protocol.LogEntry = logLevelRegex.findFirstMatchIn(line) flatMap { m =>
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
    // TODO - Figure out WHY these are being sent via system out and NULLIFY THEM.
    //   it could be this whole mechanism is just uneeded.
    //send(entry)
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

  private val consoleWriter = new Writer() {
    override def write(chars: Array[Char], offset: Int, length: Int): Unit = {
      consoleBuf.synchronized {
        consoleBuf.append(chars, offset, length);
      }
    }

    override def flush(): Unit = flushConsoleBuf

    override def close(): Unit = flushConsoleBuf
  }
  private[sbt] val consoleOut = ConsoleOut.printWriterOut(new PrintWriter(consoleWriter))
  private[sbt] val voidConsoleOut = ConsoleOut.printWriterOut(new PrintWriter(new java.io.Writer() {
    override def write(cbuf: Array[Char], off: Int, len: Int) = ()
    override def close(): Unit = ()
    override def flush(): Unit = ()
  }))
}

