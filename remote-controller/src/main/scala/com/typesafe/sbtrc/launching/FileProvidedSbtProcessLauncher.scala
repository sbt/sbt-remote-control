package com.typesafe.sbtrc
package launching

import properties.SbtRcProperties._
import java.io.File

/**
 * This is an sbt launcher where we need to provide all the necessary files for launching by hand.
 *
 * @param launcherJar
 *           The sbt launcher you wish to use when starting sbt.
 * @param probeFiles
 *           The jars necessary to launch sbt probes.   These are the packaged
 *           jar files for ui-interface + sbt-rc-controller-probe for each sbt version you
 *           wish to launch.
 */
// TODO - Add additional props argument so we can pass more JVM options down to the debug launcher.
class FileProvidedSbtProcessLauncher(
  override val sbtLauncherJar: File,
  probeFiles: Seq[File],
  optRepos: Seq[SbtPropertiesHelper.Repository] = Nil) extends BasicSbtProcessLauncher {

  // Support objects for the various sbts.
  object sbt12Support extends SbtDefaultPropsfileLaunchInfo {
    val myFiles =
      probeFiles.filter(_.getName contains "0-12") ++
        probeFiles.filter(_.getName contains "sbt-rc-props")
    override val controllerClasspath: Seq[File] = myFiles filterNot (_.getName contains "ui-interface")
    override val extraJars: Seq[File] = myFiles filter (_.getName contains "ui-interface")
    // TODO - Get this version from properties!
    override val sbtVersion = "0.12.4"
    override val optionalRepositories = optRepos
  }
  object sbt13Support extends SbtDefaultPropsfileLaunchInfo {
    val myFiles =
      probeFiles filter (_.getName contains "0-13")
    override val controllerClasspath: Seq[File] = myFiles filterNot (_.getAbsolutePath contains "ui-interface")
    override val extraJars: Seq[File] = myFiles filter (_.getAbsolutePath contains "ui-interface")
    // TODO - Get this version from properties!
    override val sbtVersion = "0.13.0-RC3"
    override val optionalRepositories = optRepos
  }

  override def getLaunchInfo(version: String): SbtBasicProcessLaunchInfo =
    version match {
      case "0.12" => sbt12Support
      case "0.13" => sbt13Support
      case _ => sys.error("Unsupported sbt version: " + version)
    }

}