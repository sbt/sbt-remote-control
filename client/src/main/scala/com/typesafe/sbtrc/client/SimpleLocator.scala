package com.typesafe.sbtrc
package client

import java.io.File
import java.net.URL

object SimpleLocator extends LaunchedSbtServerLocator {
  def sbtProperties(directory: File): URL = {
    // TODO - Pull from a resource....
    getClass.getClassLoader.getResource("sbt-server.properties")
  }
}