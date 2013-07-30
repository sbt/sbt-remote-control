package com.typesafe.sbtrc

import xsbti.AppConfiguration
import xsbti.ApplicationID
import java.io.File
import properties.SbtRcProperties._
import scala.util.control.NonFatal

/** A class we use to help us look up jars using the launcher. */
case class LookupApplicationId(name: String, mainClass: String) extends ApplicationID {
  final val groupID = "com.typesafe.sbtrc"
  def version = APP_VERSION // Cheaty way to get version from our properties file.
  final val mainComponents = Array[String]("")
  final val classpathExtra = Array[File]()
  final val crossVersioned = false
  def crossVersionedValue: xsbti.CrossValue = xsbti.CrossValue.Disabled
}

/**
 * This class is able to create the command line for Sbt child processes
 * using the launcher to discover the controller jars.
 *
 * TODO - This class should support multiple sbt versions via different shim interfaces and controller commands.
 *
 * @param configuration  The AppConfiguration passed to an application
 *                       via the SBT launcher.   We use this to lookup the controller jars.
 * @param launcherJar   The sbt launcher to use when launching sbt child processes.
 * @param optionalRepositories Name-File pairs of local repositories to use when resolving stuff.
 */
class DefaultSbtProcessLauncher(
  configuration: AppConfiguration,
  val sbtLauncherJar: File = DefaultSbtProcessLauncher.defaultLauncherLookup,
  optRepositories: Seq[(String, File, Option[String])] = Seq.empty)
  extends BasicSbtProcessLauncher {

  // The launcher interface for resolving more STUFF
  private def launcher = configuration.provider.scalaProvider.launcher

  /**
   * Our published support for sbt 0.12.
   *  TODO - clean this code up.  We should autocreate app names and scala versions based on sbt version...
   */
  object sbt012support extends SbtDefaultPropsfileLaunchInfo {
    // TODO - better property name
    override val sbtVersion = SBT_VERSION
    // The Application for the controller jars.  We can use this to get the classpath.
    private object probeApp extends LookupApplicationId(
      name = "sbt-rc-probe-0-12",
      mainClass = "com.typesafe.sbtrc.SetupSbtChild")
    private object uiPlugin extends LookupApplicationId(
      name = "sbt-rc-ui-interface-0-12",
      mainClass = "com.typesafe.sbt.ui.SbtUiPlugin")
    // This will resolve the probe artifact using our launcher and then
    // give us the classpath
    override val controllerClasspath: Seq[File] =
      launcher.app(probeApp, "2.9.2").mainClasspath
    override val extraJars: Seq[File] =
      //   This will resolve the uiContextJar artifact using our launcher and then
      // give us the classpath
      launcher.app(uiPlugin, "2.9.2").mainClasspath
    override val optionalRepositories = optRepositories
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