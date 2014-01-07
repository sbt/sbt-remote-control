package com.typesafe.sbtrc
package server

import xsbti.{ ServerMain, AppConfiguration, MainResult }
import java.net.{ URI, ServerSocket }

case class Exit(code: Int) extends xsbti.Exit

class SbtServerMain extends ServerMain {
  def start(config: AppConfiguration): xsbti.Server = {
    val server = new SbtServer(makeEngine(config), new ServerSocket(0))
    server.start()
    server
  }
  def makeEngine(config: AppConfiguration): SbtServerEngine =
    sbt.Sbt13ServerEngine(config)
}