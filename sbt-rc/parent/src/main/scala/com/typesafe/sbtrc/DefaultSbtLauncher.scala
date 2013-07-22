package com.typesafe.sbtrc

import xsbti.AppConfiguration
import xsbti.ApplicationID
import java.io.File
import properties.SbtRcProperties._
import scala.util.control.NonFatal

/**
 * This class is able to create the command line for Sbt child processes
 * using the launcher to discover the controller jars.
 *
 * TODO - This class should support multiple sbt versions via different shim interfaces and controller commands.
 *
 * @param configuration  The AppConfiguration passed to an application
 *                       via the SBT launcher.   We use this to lookup the controller jars.
 * @param launcherJar   The sbt launcher to use when launching sbt child processes.
 */
class DefaultSbtProcessLauncher(
  configuration: AppConfiguration,
  val sbtLauncherJar: File = DefaultSbtProcessLauncher.defaultLauncherLookup)
  extends BasicSbtProcessLauncher {

  // The launcher interface for resolving more STUFF
  private def launcher = configuration.provider.scalaProvider.launcher

  /**
   * Our published support for sbt 0.12.
   *  TODO - clean this code up.
   */
  object sbt012support extends SbtBasicProcessLaunchInfo {
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
    lazy val controllerClasspath: Seq[File] =
      launcher.app(probeApp, SBT_SCALA_VERSION).mainClasspath
    private lazy val uiContextClassPathFor012: Seq[File] =
      //   This will resolve the uiContextJar artifact using our launcher and then
      // give us the classpath
      launcher.app(uiPlugin, SBT_SCALA_VERSION).mainClasspath
    // Here we write out our startup props file
    // What we use this for is to hack
    lazy val propsFile = {
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
  resources: ${uiContextClassPathFor012 map (_.getCanonicalPath) mkString ","}

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
  }

  override def getLaunchInfo(version: String): SbtBasicProcessLaunchInfo =
    version match {
      case "0.12" => sbt012support
      case _ => sys.error(s"sbt version $version is not supported!")
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