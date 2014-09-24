
package sbt.client.impl

import java.io.File
import java.net.URL

object SimpleLocator extends LaunchedSbtServerLocator {
  def sbtProperties(directory: File): URL = {
    // TODO - Check sbt binary/full version first!
    getClass.getClassLoader.getResource("sbt-server.properties")
  }
}