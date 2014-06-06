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

  private def withTemporaryFileSavingTemps[T](prefix: String, postfix: String)(action: File => T): T = {
    val file = File.createTempFile(prefix, postfix)
    try { action(file) }
    finally {
      if (System.getenv("SBT_SERVER_SAVE_TEMPS") eq null)
        file.delete()
    }
  }

  final override def locate(directory: File): URI = {
    // TODO - Windows friendly.
    withTemporaryFileSavingTemps("boot", "properties") { propsFile =>
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
            import scala.collection.JavaConverters._
            // readLines should get EOF since we got EOF on stdout...
            val errors = sbt.IO.readLines(error)
            sys.error(s"Failed to start server, error output was: '${errors.mkString("\n")}' directory '${pb.directory}' command '${pb.command.asScala.mkString(" ")}'")
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
    // Now we should look for an embedded launcher we can extract somewhere.
    dumpLauncherFromJar orElse
      findLauncherReflectively getOrElse sys.error("Unable to find sbt launcher.jar file.")

  // Attempts to grab the JAR for the sbt launcher reflectively from
  // the classloader of launcher classes.
  def findLauncherReflectively: Option[File] =
    // First check the classloader we're currently running in. This works for launched applicatons.
    findLauncherClassloader(getClass.getClassLoader) orElse
      // Now check to see if we're running in an isolated classloader BUT from a launched applciation.
      findLauncherClassloader(ClassLoader.getSystemClassLoader)

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

  private def openNestedLauncherStream: Option[java.io.InputStream] =
    Option(getClass.getClassLoader.getResourceAsStream("sbt-launch.jar"))
  // TODO - maybe we have the build run this and store it in a property somewhere.
  lazy val nestedLauncherSha: Option[String] = openNestedLauncherStream flatMap { in =>
    try Some(sha1(in))
    catch {
      case NonFatal(e) => None
    }
  }

  def dumpLauncherFromJar: Option[File] = nestedLauncherSha flatMap { sha =>
    val userHome = new File(sys.props("user.home"))
    val sbtHome = sys.props.get("sbt.global.base") map (new File(_)) getOrElse new File(userHome, ".sbt")
    val launchHome = new File(sbtHome, "launchers")
    // TODO - maybe we need to clean old launchers...
    val launcher = new File(launchHome, s"sbt-launch-${sha}.jar")
    if (launcher.exists) Some(launcher)
    else try {
      for {
        stream <- openNestedLauncherStream
        _ = sbt.IO.transfer(stream, launcher)
      } yield launcher
    } catch {
      case NonFatal(e) => None
    }
  }

  // TODO - move this HASHING stuff somewhere useful

  // This should calculate the SHA sum of a file the same as the linux process.
  private def sha1(in: java.io.InputStream): String =
    digest(java.security.MessageDigest.getInstance("SHA-1"))(in)
  private def digest(digest: java.security.MessageDigest)(in: java.io.InputStream): String = {
    val buffer = new Array[Byte](8192)
    try {
      def read(): Unit = in.read(buffer) match {
        case x if x <= 0 => ()
        case size => digest.update(buffer, 0, size); read()
      }
      read()
    } finally in.close()
    val sha = convertToHex(digest.digest())
    sha
  }
  private def convertToHex(data: Array[Byte]): String = {
    val buf = new StringBuffer
    def byteToHex(b: Int) =
      if ((0 <= b) && (b <= 9)) ('0' + b).toChar
      else ('a' + (b - 10)).toChar
    for (i <- 0 until data.length) {
      buf append byteToHex((data(i) >>> 4) & 0x0F)
      buf append byteToHex(data(i) & 0x0F)
    }
    buf.toString
  }

}