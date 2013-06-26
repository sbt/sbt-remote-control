package com.typesafe.sbtrc

import xsbti.AppConfiguration
import xsbti.ApplicationID
import java.io.File
import properties.SbtRcProperties._
import scala.util.control.NonFatal

/**
 * A trait which can create the SBT child process
 * arguments.   Note:  Since we need access to the launcher, for
 * distributed SNAP, we make this something that can be passed in and extended
 * so that we can have a stub implementation.
 */
trait SbtProcessLauncher {
  def arguments(port: Int): Seq[String]
}

/**
 * This class is able to create the command line for Sbt child processes
 * using the launcher to discover the controller jars.
 *
 * @param configuration  The AppConfiguration passed to an application
 *                       via the SBT launcher.   We use this to lookup the controller jars.
 * @param launcherJar   The sbt launcher to use when launching sbt child processes.
 */
class DefaultSbtProcessLauncher(
  configuration: AppConfiguration,
  launcherJar: File = DefaultSbtProcessLauncher.defaultLauncherLookup) extends SbtProcessLauncher {
  // The launcher interface for resolving more STUFF
  private def launcher = configuration.provider.scalaProvider.launcher
  // The Application for the controller jars.  We can use this to get the classpath.
  private object probeApp extends ApplicationID {
    // TODO - Pull these constants from some build-generated properties or something.
    def groupID = "com.typesafe.sbtrc"
    def name = "sbt-rc-controller"
    def version = APP_VERSION // Cheaty way to get version
    def mainClass = "com.typesafe.sbtrc.SetupSbtChild" // TODO - What main class?
    def mainComponents = Array[String]("") // TODO - is this correct.
    def crossVersioned = false
    def classpathExtra = Array[File]()
  }

  private object uiPlugin extends ApplicationID {
    // TODO - Pull these constants from some build-generated properties or something.
    def groupID = "com.typesafe.sbtrc"
    def name = "sbt-shim-ui-interface"
    def version = APP_VERSION // Cheaty way to get version
    def mainClass = "com.typesafe.sbt.ui.SbtUiPlugin" // TODO - What main class?
    def mainComponents = Array[String]("") // TODO - is this correct.
    def crossVersioned = false
    def classpathExtra = Array[File]()
  }

  // This will resolve the probe artifact using our launcher and then
  // give us the classpath
  private lazy val probeClassPath: Seq[File] =
    launcher.app(probeApp, SBT_SCALA_VERSION).mainClasspath

  private lazy val uiContextClassPath: Seq[File] =
    //   This will resolve the uiContextJar artifact using our launcher and then
    // give us the classpath
    launcher.app(uiPlugin, SBT_SCALA_VERSION).mainClasspath

  // Here we write out our startup props file
  // What we use this for is to hack
  private lazy val propsFile = {
    val tmp = File.createTempFile("sbtrc", "properties")
    val writer = new java.io.BufferedWriter(new java.io.FileWriter(tmp))
    try {
      writer.write(s"""
[scala]
  version: auto

[app]
  org: org.scala-sbt
  name: sbt
  version: ${SBT_VERSION}
  class: sbt.xMain
  components: xsbti,extra
  cross-versioned: false
  resources: ${uiContextClassPath map (_.getCanonicalPath) mkString ","}

[repositories]
  local
  typesafe-ivy-releases: http://repo.typesafe.com/typesafe/ivy-releases/, [organization]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly
  typesafe-ivy-snapshots: http://repo.typesafe.com/typesafe/ivy-snapshots/, [organization]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly
  maven-central

[boot]
 directory: $${sbt.boot.directory-$${sbt.global.base-$${user.home}/.sbt}/boot/}

[ivy]
  ivy-home: $${sbt.ivy.home-$${user.home}/.ivy2/}
  checksums: $${sbt.checksums-sha1,md5}
  override-build-repos: $${sbt.override.build.repos-false}
  repository-config: $${sbt.repository.config-$${sbt.global.base-$${user.home}/.sbt}/repositories}
""")
    } finally {
      writer.close()
    }
    tmp.deleteOnExit()
    tmp
  }

  def arguments(port: Int): Seq[String] = {
    val portArg = "-Dsbtrc.control-port=" + port.toString
    // TODO - These need to be configurable *and* discoverable.
    // we have no idea if computers will be able to handle this amount of
    // memory....
    val defaultJvmArgs = Seq(
      "-Xss1024K",
      "-Xmx" + SBT_XMX,
      "-XX:PermSize=" + SBT_PERMSIZE,
      "-XX:+CMSClassUnloadingEnabled")
    // TODO - handle spaces in strings and such...
    val sbtProps = Seq(
      // TODO - Remove this junk once we don't have to hack our classes into sbt's classloader.
      "-Dsbt.boot.properties=" + propsFile.getAbsolutePath,
      portArg)
    // TODO - Can we look up the launcher.jar via a class?
    val jar = Seq("-jar", launcherJar.getAbsolutePath)

    // TODO - Is the cross-platform friendly?
    val probeClasspathString =
      "\"\"\"" + ((probeClassPath map (_.getAbsolutePath)).distinct mkString File.pathSeparator) + "\"\"\""
    val escapedPcp = probeClasspathString.replaceAll("\\\\", "/")
    val sbtcommands = Seq(
      s"apply -cp $escapedPcp com.typesafe.sbtrc.SetupSbtChild",
      "listen")

    val result = Seq("java") ++
      defaultJvmArgs ++
      sbtProps ++
      jar ++
      sbtcommands

    System.err.println("Running sbt-child with arguments =\n\t" + result.mkString("\n\t"))

    result
  }
}
object DefaultSbtProcessLauncher {
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
