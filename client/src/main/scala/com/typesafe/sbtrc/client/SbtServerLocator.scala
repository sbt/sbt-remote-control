package com.typesafe.sbtrc
package client

import java.io.File
import java.net.{ URI, URL }
import xsbti.AppConfiguration
import scala.util.control.NonFatal

/**
 * This class is responsible for determining where the sbt server is located.
 */
trait SbtServerLocator {
  /** Given a project directory, look up the URI where the server is running. */
  def locate(directory: File): URI
}

abstract class AbstractSbtServerLocator extends SbtServerLocator {
  def sbtLaunchJar: File
  def sbtProperties(directory: File): URL

  final override def locate(directory: File): URI = {
    // TODO - Windows friendly.
    sbt.IO.withTemporaryFile("boot", "properties") { propsFile =>
      sbt.IO.download(sbtProperties(directory), propsFile)
      // TODO - Friendly java lookup and such
      val pb = new ProcessBuilder(
        "java", "-jar", sbtLaunchJar.getCanonicalPath,
        "--locate",
        s"@${propsFile.toURI.toURL.toString}")
      pb.directory(directory)
      val process = pb.start()
      process.getOutputStream.close()
      val input = process.getInputStream
      process.getErrorStream.close()
      readUntilSynch(new java.io.BufferedReader(new java.io.InputStreamReader(input))) match {
        case Some(uri) => uri
        case _ => sys.error("Failed to start server!")
      }
    }
  }
  private def readUntilSynch(in: java.io.BufferedReader): Option[URI] = {
    def read(): Option[URI] = in.readLine match {
      case null => None
      // TODO - Just keep reading lines until one parses.
      case uri => Some(new java.net.URI(uri))
    }
    try read()
    finally in.close()
  }
}

abstract class LaunchedSbtServerLocator extends AbstractSbtServerLocator {
  def sbtLaunchJar: File = LaunchedSbtServerLocator.defaultLauncherLookup
}
object LaunchedSbtServerLocator {
  def defaultLauncherLookup: File =
    findLauncherReflectively getOrElse sys.error("Unable to find sbt launcher.jar file.")

  // Attempts to grab the JAR for the sbt launcher reflectively from
  // the classloader of launcher classes.
  def findLauncherReflectively: Option[File] =
    try {
      val classInLauncher = classOf[AppConfiguration]
      for {
        domain <- Option(classInLauncher.getProtectionDomain)
        source <- Option(domain.getCodeSource)
        location = source.getLocation
      } yield new java.io.File(location.toURI)
    } catch {
      case NonFatal(e) => None
    }
}