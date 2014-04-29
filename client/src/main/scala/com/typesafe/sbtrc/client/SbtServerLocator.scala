package com.typesafe.sbtrc
package client

import java.io.File
import java.net.{ URI, URL }
import xsbti.AppConfiguration
import scala.util.control.NonFatal
import sbt.client._
import java.net.URISyntaxException
import scala.annotation.tailrec

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
      val input = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
      val error = new java.io.BufferedReader(new java.io.InputStreamReader(process.getErrorStream))

      try {
        readUntilSynch(input) match {
          case Some(uri) =>
            uri
          case _ =>
            // readLines should get EOF since we got EOF on stdout...
            val errors = sbt.IO.readLines(error)
            sys.error(s"Failed to start server, error output was: '${errors.mkString("\n")}'")
        }
      } finally {
        try input.close()
        finally error.close()
      }
    }
  }
  @tailrec
  private def readUntilSynch(in: java.io.BufferedReader): Option[URI] = in.readLine match {
    case null => None
    case uri => try Some(new java.net.URI(uri))
    catch {
      case e: URISyntaxException =>
        // TODO should there be some limit on number of "bad" lines we parse?
        // TODO should we include "bad" lines in the error log on eventual failure?
        readUntilSynch(in)
    }
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
    // First check the classloader we're currently running in. This works for launched applicatons.
    findLauncherClassloader(getClass.getClassLoader) orElse
      // Now check to see if we're running in an isolated classloader BUT from a launched applciation.
      findLauncherClassloader(ClassLoader.getSystemClassLoader) orElse
      // Now we should look for an embedded launcher we can extract somewhere.
      dumpLauncherFromJar

  def findLauncherClassloader(cl: ClassLoader): Option[File] = {
    try {
      // TODO - Maybe we shouldn't hardcode the launcher class...
      // "xsbt.boot.Boot"
      val classInLauncher = cl.loadClass("xsbti.AppConfiguration")
      for {
        domain <- Option(classInLauncher.getProtectionDomain)
        source <- Option(domain.getCodeSource)
        location = source.getLocation
        // Ignore the launcher interface itself.
        file = new java.io.File(location.toURI)
        if !(file.getName contains "launcher-interface.jar")
      } yield file
    } catch {
      case NonFatal(e) => None
    }
  }

  def dumpLauncherFromJar: Option[File] = {
    val userHome = new File(sys.props("user.home"))
    val rcHome = new File(userHome, ".sbtrc")
    // TODO - Autoupdate this guy
    val launcher = new File(rcHome, "sbt-launch.jar")
    if (launcher.exists) Some(launcher)
    else try {
      for {
        stream <- Option(getClass.getClassLoader.getResourceAsStream("sbt-launch.jar"))
        _ = sbt.IO.transfer(stream, launcher)
      } yield launcher
    } catch {
      case NonFatal(e) => None
    }
  }

}