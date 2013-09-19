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
  case class Sbt12Support(sbtVersion: String) extends SbtDefaultPropsfileLaunchInfo {
    val myFiles =
      probeFiles.filter(_.getName contains "0-12") ++
        probeFiles.filter(_.getName contains "sbt-rc-props")
    override val controllerClasspath: Seq[File] = myFiles filterNot (_.getName contains "ui-interface")
    override val extraJars: Seq[File] = myFiles filter (_.getName contains "ui-interface")
    // TODO - Get this version from properties!
    override val optionalRepositories = optRepos
  }
  case class Sbt13Support(sbtVersion: String) extends SbtDefaultPropsfileLaunchInfo {
    val myFiles =
      probeFiles.filter(_.getName contains "0-13") ++
        probeFiles.filter(_.getName contains "sbt-rc-props")
    override val controllerClasspath: Seq[File] = myFiles filterNot (_.getAbsolutePath contains "ui-interface")
    override val extraJars: Seq[File] = myFiles filter (_.getAbsolutePath contains "ui-interface")
    // TODO - Get this version from properties!
    override val optionalRepositories = optRepos
  }

  override def getLaunchInfo(version: String, fullVersion: String): SbtBasicProcessLaunchInfo =
    version match {
      // TODO - We only support 0.12.4+
      case "0.12" => Sbt12Support(fullVersion)
      case "0.13" => Sbt13Support(fullVersion)
      case _ => sys.error("Unsupported sbt version: " + version)
    }

}
