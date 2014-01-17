package sbt
package server

import com.typesafe.sbtrc.protocol
import scala.util.matching.Regex
import java.io.Writer
import java.io.PrintWriter

// Our replacement for the global logger that allows you to swap out who is listening to events.
private[sbt] object EventLogger extends Logger {
  @volatile
  private var client: SbtClient = NullSbtClient
  @volatile
  private var peer: Option[String => Unit] = None

  def updateClient(next: SbtClient): Unit = client = next
  def updatePeer(f: String => Unit): Unit = peer = Some(f)

  def send(entry: protocol.LogEntry): Unit = {
    client.send(protocol.LogEvent(entry))
    peer match {
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
        case Some(level) => Some(protocol.LogMessage(level.toString, message))
        case None => levelString match {
          case "success" => Some(protocol.LogSuccess(message))
          case _ => None
        }
      }
    } getOrElse {
      protocol.LogMessage(Level.Info.toString, line)
    }
    send(entry)
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

  private[sbt] val consoleOut = ConsoleLogger.printWriterOut(new PrintWriter(consoleWriter))
}