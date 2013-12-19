package com.typesafe.sbtrc
package server

import xsbti.{ ServerMain, AppConfiguration, MainResult }
import java.net.{ URI, ServerSocket }

case class Exit(code: Int) extends xsbti.Exit
class SbtServerMain extends ServerMain {
  private var server: SbtServer = null

  def start(config: AppConfiguration): URI = {
    val socket = new ServerSocket(0)
    val port = socket.getLocalPort
    val addr = socket.getInetAddress.getHostAddress
    server = new SbtServer(makeEngine(config), socket)
    server.start()
    new URI(s"http://${addr}:${port}")
  }

  def awaitTermination(): MainResult = {
    // Wait for the server to stop, then exit.
    server.join()
    // TODO - We should allow the server to tell us to reboot.
    Exit(0)
  }

  def makeEngine(config: AppConfiguration): SbtServerEngine =
    sbt.Sbt13ServerEngine(config)
}