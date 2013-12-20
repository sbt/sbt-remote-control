package com.typesafe.sbtrc
package server
package shims

import java.nio.charset.Charset
import java.io.PrintStream

/**
 * This class can be used to construct PrintStreams which are implemented
 * backed on a simple function.
 */
private[shims] class LoggedOutputStream(logger: String => Unit, charset: Charset = Charset.defaultCharset) extends java.io.OutputStream {
  private val buf = new collection.mutable.ArrayBuffer[Byte] 
  override def write(b: Int): Unit = buf.append(b.toByte)
  override def flush(): Unit = {
    val array = buf.toArray
    logger(new String(array, charset))
    buf.clear()
  }
  // TODO - Do something useful on close...
  override def close(): Unit = {}
}
object LoggedPrintStream {
  def apply(logger: String => Unit)(implicit charset: Charset = Charset.defaultCharset): PrintStream = {
    val output = new LoggedOutputStream(logger, charset)
    new java.io.PrintStream(output, true)
  }
}
object SystemShims {
  def replaceOutput(stdout: String => Unit, stderr: String => Unit): Unit = {
    val newOut = LoggedPrintStream(stdout)
    val newErr = LoggedPrintStream(stderr)
    System.setOut(newOut)
    System.setErr(newErr)
  }
}