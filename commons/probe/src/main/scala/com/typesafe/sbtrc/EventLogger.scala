package com.typesafe.sbtrc

import _root_.sbt._
import scala.util.matching.Regex
import java.io.PrintWriter
import java.io.Writer

// requestSerial would be 0 for "not during a request"
private[sbtrc] class EventLogger(client: ipc.Client, requestSerial: Long) extends Logger {
    def send(entry: protocol.LogEntry) = {
      client.replyJson(requestSerial, protocol.LogEvent(entry))
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

    private[sbtrc] val consoleOut = ConsoleLogger.printWriterOut(new PrintWriter(consoleWriter))
  }