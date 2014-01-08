package com.typesafe.sbtrc
package server

import java.io._
/**
 * This is meant to be a rotating file log that can auto-squash its contents if they get too
 * big.  Right now we just dump a ton of information for debugging purposes.
 */
class FileLogger(file: File) {
  // make sure we can write to the file.
  file.getParentFile.mkdirs()
  private val stream = new PrintWriter(new FileWriter(file))
  
  def log(msg: String): Unit = {
    stream.write(msg)
    stream.write("\n")
    stream.flush()
  }
  
  def error(msg: String,e: Throwable): Unit = {
    stream.write(msg)
    stream.write("\n")
    e.printStackTrace(stream)
    stream.flush()
  }
  
  def close(): Unit = stream.close()
}