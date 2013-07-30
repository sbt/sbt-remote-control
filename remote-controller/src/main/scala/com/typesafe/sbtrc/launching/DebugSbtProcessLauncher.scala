package com.typesafe.sbtrc
package launching

import properties.SbtRcProperties._
import java.io.File

// Helper to load the environemnt for DebugSbtProcessLauncher
object EnvironmentLoader {
  private val probeClassPathProp = "sbtrc.controller.classpath"
  private val sbtLauncherJarProp = "sbtrc.launch.jar"
  private val allNeededProps = Seq(probeClassPathProp, sbtLauncherJarProp)
  private def assertPropsArentMissing(): Unit = {
    val missing = allNeededProps.filter(sys.props(_) eq null)
    if (missing.nonEmpty)
      throw new RuntimeException("DebugSbtChildProcessMaker requires system props: " + missing)
  }

  def sbtLaunchJar: File = {
    assertPropsArentMissing()
    new File(sys.props(sbtLauncherJarProp))
  }

  def probeFiles: Seq[File] = {
    assertPropsArentMissing()
    (sys.props(probeClassPathProp) split File.pathSeparator map (n => new File(n)))(collection.breakOut)
  }

}
/**
 * This guy is responsible for finding all the information we need to run sbtrc
 * via the a default set of environment variables...
 */
object DebugSbtProcessLauncher extends FileProvidedSbtProcessLauncher(EnvironmentLoader.sbtLaunchJar, EnvironmentLoader.probeFiles)
