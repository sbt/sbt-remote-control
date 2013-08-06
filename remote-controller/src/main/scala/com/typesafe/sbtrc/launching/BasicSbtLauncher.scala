package com.typesafe.sbtrc
package launching

import java.io.File
import properties.SbtRcProperties._
import scala.collection.JavaConverters.seqAsJavaListConverter

/**
 * This class contains all the sbt-version specific information we
 * need to launch sbt.
 */
trait SbtBasicProcessLaunchInfo {
  /** The properties file to pass sbt when launching. */
  def propsFile: File
  /** The controller classpath used to hook sbt. */
  def controllerClasspath: Seq[File]
  // TODO - Controller main class?
  // TODO - Launcher jar?
}

/**
 * This class uses the sbt properties helper to generate (lazily) the properties
 *  file used to launch sbt.
 */
trait SbtDefaultPropsfileLaunchInfo extends SbtBasicProcessLaunchInfo {
  /** The sbt version to use in the properties file. */
  def sbtVersion: String
  /** Extra jars/directories to place on sbt's classpath. */
  def extraJars: Seq[File] = Nil
  /** Optional repositories to use when resolvign sbt. */
  def optionalRepositories: Seq[SbtPropertiesHelper.Repository] = Nil

  final override lazy val propsFile = {
    val tmp = File.createTempFile("sbtrc", "properties")
    // TODO - Get this version from properties!
    SbtPropertiesHelper.makePropertiesFile(tmp, sbtVersion, extraJars, optionalRepositories)
    tmp.deleteOnExit()
    tmp
  }
}

/** A "template" for how to create an sbt process launcher. */
trait BasicSbtProcessLauncher extends SbtProcessLauncher {
  override def apply(cwd: File, port: Int): ProcessBuilder = {
    import collection.JavaConverters._
    new ProcessBuilder(arguments(cwd, port).asJava).
      directory(cwd)
  }

  /** Default JVM Args to use. */
  def jvmArgs: Seq[String] =
    Seq(
      "-Xss1024K",
      "-Xmx" + SBT_XMX,
      "-XX:PermSize=" + SBT_PERMSIZE,
      "-XX:+CMSClassUnloadingEnabled")

  /**
   * Returns the versoin specific information
   *  launch sbt for the given project.
   *  @param version The sbt binary version to use
   */
  def getLaunchInfo(version: String): SbtBasicProcessLaunchInfo

  /** Returns an sbt launcher jar we can use to launch this process. */
  def sbtLauncherJar: File

  /** Returns the sbt binary version we're about to fork. */
  def getSbtBinaryVersion(cwd: File): String =
    io.SbtVersionUtil.findProjectBinarySbtVersion(cwd).getOrElse("0.12")

  /**
   * Generates the arguments used by the sbt process launcher.
   */
  def arguments(cwd: File, port: Int): Seq[String] = {
    val portArg = "-Dsbtrc.control-port=" + port.toString
    // TODO - These need to be configurable *and* discoverable.
    // we have no idea if computers will be able to handle this amount of
    // memory....
    val defaultJvmArgs = jvmArgs
    val sbtBinaryVersion = getSbtBinaryVersion(cwd)
    val info = getLaunchInfo(sbtBinaryVersion)
    // TODO - handle spaces in strings and such...
    val sbtProps = Seq(
      // TODO - Remove this junk once we don't have to hack our classes into sbt's classloader.
      "-Dsbt.boot.properties=" + info.propsFile.toURI.toASCIIString,
      portArg)
    // TODO - Can we look up the launcher.jar via a class?
    val jar = Seq("-jar", sbtLauncherJar.getAbsolutePath)

    // TODO - Is the cross-platform friendly?
    val probeClasspathString =
      "\"\"\"" + ((info.controllerClasspath map (_.getAbsolutePath)).distinct mkString File.pathSeparator) + "\"\"\""
    val escapedPcp = probeClasspathString.replaceAll("\\\\", "/")
    val sbtcommands = Seq(
      s"apply -cp $escapedPcp com.typesafe.sbtrc.SetupSbtChild",
      "listen")

    val result = Seq("java") ++
      defaultJvmArgs ++
      sbtProps ++
      jar ++
      sbtcommands

    result
  }
}