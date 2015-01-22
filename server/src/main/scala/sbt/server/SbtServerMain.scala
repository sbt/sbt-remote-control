package sbt.server

import xsbti.{ ServerMain, AppConfiguration, MainResult }
import java.net.{ URI, ServerSocket }

final case class Exit(code: Int) extends xsbti.Exit

class SbtServerMain extends ServerMain {
  // Make an instance before we use this in a thread to avoid concurrency issue
  val serializations = DynamicSerialization.defaultSerializations

  def start(config: AppConfiguration): xsbti.Server = {
    System.err.println(s"Starting sbt server...")
    // Clean the log directory first....
    val serverDir = new java.io.File(".sbtserver")
    // for debugging, it's useful to save the old log file
    val oldLogFile = new java.io.File(serverDir, "master.log")
    val previousLogFile = new java.io.File(serverDir, "previous.log")
    if (oldLogFile.exists)
      sbt.IO.move(oldLogFile, previousLogFile)
    for (f <- sbt.IO.listFiles(serverDir)) {
      if (f.getName != "previous.log")
        sbt.IO.delete(f)
    }
    new SbtServer(config, new ServerSocket(0))
  }
}
