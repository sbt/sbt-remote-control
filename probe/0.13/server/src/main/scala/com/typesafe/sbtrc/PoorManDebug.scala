package com.typesafe.sbtrc

object PoorManDebug {
  // TODO - is this cool? I dunno, but let's do it ANYWAY.
  @volatile
  private var out: java.io.PrintStream = System.err

  val isDebugEnabled: Boolean =
    Option(sys.props("sbt.probe.debug")).filter(_.toLowerCase == "true").isDefined

  val isTraceEnabled: Boolean =
    Option(sys.props("sbt.probe.trace")).filter(_.toLowerCase == "true").isDefined

  def debug(msg: Any): Unit =
    if (isDebugEnabled || isTraceEnabled) {
      out.println("PROBE-DEBUG: " + msg)
    }

  def trace(msg: Any): Unit =
    if (isTraceEnabled) {
      out.println("PROBE-TRACE: " + msg)
    }
}