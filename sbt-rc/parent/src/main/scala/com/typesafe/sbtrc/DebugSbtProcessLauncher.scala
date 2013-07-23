package com.typesafe.sbtrc

import properties.SbtRcProperties._
import java.io.File

/**
 * This guy is responsible for finding all the information we need to run sbtrc in its own build
 * so that we can run tests.  Also useful for consumers who need to run tests outside of an sbt-launched application.
 */
// TODO - Add additional props argument so we can pass more JVM options down to the debug launcher.
object DebugSbtProcessLauncher extends BasicSbtProcessLauncher {

  private val probeClassPathProp = "sbtrc.controller.classpath"
  private val sbtLauncherJarProp = "sbtrc.launch.jar"
  private val allNeededProps = Seq(probeClassPathProp, sbtLauncherJarProp)

  // NOTE -> THIS HAS TO BE LAZY
  // These values are only available when we run a debug build locally.
  // When trying to run the UI now, things explode because the script (appropriately)
  // Does not specify these things.
  private def assertPropsArentMissing(): Unit = {
    val missing = allNeededProps.filter(sys.props(_) eq null)
    if (missing.nonEmpty)
      throw new RuntimeException("DebugSbtChildProcessMaker requires system props: " + missing)
  }

  private lazy val probeClassPath: Seq[File] = (sys.props(probeClassPathProp) split File.pathSeparator map (n => new File(n)))(collection.breakOut)
  private lazy val commandClasspath: Seq[File] = probeClassPath filterNot (_.getAbsolutePath contains "ui-interface")
  private lazy val uiClassDir: Seq[File] = probeClassPath filter (_.getAbsolutePath contains "ui-interface")
  lazy val sbtLauncherJar: File = new File(sys.props(sbtLauncherJarProp))

  /** An sbt process launch info that just pulls from the environment. */
  object sbtLocalSupport extends SbtBasicProcessLaunchInfo {
    override def controllerClasspath: Seq[File] = commandClasspath
    override lazy val propsFile = {
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
  resources: ${uiClassDir map (_.getCanonicalPath) mkString ","}

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
    sbtLocalSupport
}
