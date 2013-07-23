import sbt._
import SbtRcBuild._
import Dependencies._
import IvyRepositories.{localRepoArtifacts}
// NOTE - This file is only used for SBT 0.12.x, in 0.13.x we'll use build.sbt and scala libraries.
// As such try to avoid putting stuff in here so we can see how good build.sbt is without build.scala.


object TheBuild extends Build {

  // ADD sbt launcher support here.
  override def settings = super.settings ++ SbtSupport.buildSettings ++ baseVersions

  val root = (
    Project("root", file("."))  // TODO - Oddities with clean..
    aggregate((publishedProjects.map(_.project) ++ Seq(itRunner.project, itTests.project)):_*)
    settings(
      // Stub out commands we run frequently but don't want them to really do anything.
      Keys.publish := {},
      Keys.publishLocal := {}
    )
  )

  // These are the projects we want in the local repository we deploy.
  lazy val publishedSbtShimProjects = Set(playShimPlugin, eclipseShimPlugin, ideaShimPlugin, sbtUiInterface, defaultsShimPlugin)
  lazy val publishedProjects: Seq[Project] = Seq(sbtRemoteController, sbtDriver, props) ++ publishedSbtShimProjects

  // TODO - This should be the default properties we re-use between the controller and the driver.
  lazy val props = (
    PropsProject("props")
    settings(Properties.makePropertyClassSetting(Dependencies.sbtVersion, Dependencies.scalaVersion):_*)
  )

  // Generic UI we use in all our shims and in the remote control to execute SBT as a UI.
  lazy val sbtUiInterface = (
      SbtShimPlugin("ui-interface")
      settings(
          Keys.scalaVersion := Dependencies.sbtPluginScalaVersion, 
          Keys.scalaBinaryVersion <<= Keys.scalaVersion,
          Keys.crossVersion := CrossVersion.Disabled,
          Keys.projectID <<=  Keys.projectID apply { id =>
            id.copy(extraAttributes = Map.empty)
          })
      dependsOnRemote(sbtControllerDeps(Dependencies.sbtVersion):_*)
  )

  // This is the embedded controller for sbt projects.
  lazy val sbtRemoteController = (
    SbtRemoteControlProject("controller")
    settings(Keys.scalaVersion := Dependencies.sbtPluginScalaVersion, Keys.scalaBinaryVersion <<= Keys.scalaVersion)
    dependsOnSource("../protocol")
    dependsOn(props, sbtUiInterface % "provided")
    dependsOnRemote(
      sbtControllerDeps(Dependencies.sbtVersion):_*
    )
    settings(requiredJars(props, sbtUiInterface))
  )

  // SBT Shims
  lazy val playShimPlugin = (
    SbtShimPlugin("play")
    dependsOn(sbtUiInterface)
    dependsOnRemote(playSbtPlugin)
  )

  lazy val eclipseShimPlugin = (
    SbtShimPlugin("eclipse")
    dependsOn(sbtUiInterface)
    dependsOnRemote(eclipseSbtPlugin)
  )

  lazy val ideaShimPlugin = (
    SbtShimPlugin("idea")
    dependsOn(sbtUiInterface)
    dependsOnRemote(ideaSbtPlugin)
  )

  lazy val defaultsShimPlugin = (
    SbtShimPlugin("defaults")
    // TODO - can we just depend on all the other plugins so we only have one shim?
  )

  val verboseSbtTests = Option(sys.props("sbtrc.verbose.tests")).map(_ == "true").getOrElse(false)

  def configureSbtTest(testKey: Scoped) = Seq(
    // set up embedded sbt for tests, we fork so we can set
    // system properties.
    Keys.fork in testKey := true,
    Keys.javaOptions in testKey <<= (
      SbtSupport.sbtLaunchJar,
      Keys.javaOptions in testKey,
      requiredClasspath in sbtRemoteController,
      Keys.compile in Compile in sbtRemoteController) map {
      (launcher, oldOptions, controllerCp, _) =>
        oldOptions ++ Seq("-Dsbtrc.no-shims=true",
                          "-Dsbtrc.launch.jar=" + launcher.getAbsoluteFile.getAbsolutePath,
                          "-Dsbtrc.controller.classpath=" + Path.makeString(controllerCp.files)) ++
      (if (verboseSbtTests)
        Seq("-Dakka.loglevel=DEBUG",
            "-Dakka.actor.debug.autoreceive=on",
            "-Dakka.actor.debug.receive=on",
            "-Dakka.actor.debug.lifecycle=on")
       else
         Seq.empty)
    })


  // This project is used to drive sbt processes, installing the controller.
  lazy val sbtDriver = (
    SbtRemoteControlProject("parent")
    settings(Keys.libraryDependencies <+= (Keys.scalaVersion) { v => "org.scala-lang" % "scala-reflect" % v })
    settings(
      Keys.publishArtifact in (Test, Keys.packageBin) := true 
    )
    dependsOnSource("../protocol")
    dependsOn(props)
    dependsOnRemote(akkaActor,
                    sbtLauncherInterface,
                    sbtIo)
    settings(configureSbtTest(Keys.test): _*)
    settings(configureSbtTest(Keys.testOnly): _*)
  )

  // Set up a repository that has all our dependencies.
  import Project.Initialize

  lazy val itTests: Project = (
    SbtRemoteControlProject("integration-tests")
    dependsOnRemote(sbtLauncherInterface, sbtIo)
    dependsOn(sbtDriver, props)
    settings(
      //com.typesafe.sbtidea.SbtIdeaPlugin.ideaIgnoreModule := true,
      Keys.publish := {}
    )
  )

  lazy val itRunner: Project = (
    SbtRemoteControlProject("it-runner")
    settings(integration.settings(publishedProjects :+ itTests, itTests): _*)
    settings(
      //com.typesafe.sbtidea.SbtIdeaPlugin.ideaIgnoreModule := true,
      Keys.publish := {},
      Keys.publishLocal := {},
      // Additional dependencies required to run tests:
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.10.1",
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.9.2"
    )
  )
  
}
