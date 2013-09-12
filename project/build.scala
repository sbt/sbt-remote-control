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
  lazy val sbt12ProbeProjects = Set(playShimPlugin, sbtUiInterface, defaultsShimPlugin, sbtControllerProbe)
  lazy val sbt13ProbeProjects = Set(sbtUiInterface13, sbtControllerProbe13)
  lazy val publishedProjects: Seq[Project] = Seq(sbtRemoteController, props) ++ sbt12ProbeProjects ++ sbt13ProbeProjects

  // TODO - This should be the default properties we re-use between the controller and the driver.
  lazy val props = (
    PropsProject("props")
    settings(Properties.makePropertyClassSetting(Dependencies.sbt12Version, Dependencies.scalaVersion):_*)
  )


  // ================= 0.12 shims ==========================

  // Generic UI we use in all our shims and in the remote control to execute SBT as a UI.
  lazy val sbtUiInterface = (
      SbtShimPlugin("ui-interface", sbt12Version)
      settings(noCrossVersioning:_*)
      dependsOnSource("commons/ui-interface")
      dependsOnRemote(sbtControllerDeps(sbt12Version):_*)
  )

  // This is the embedded controller for sbt projects.
  lazy val sbtControllerProbe = (
    SbtProbeProject("probe", sbt12Version)
    dependsOnSource("commons/protocol")
    dependsOnSource("commons/probe")
    dependsOn(props, sbtUiInterface % "provided")
    dependsOnRemote(
      sbtControllerDeps(sbt12Version):_*
    )
    settings(requiredJars(props, sbtUiInterface))
    settings(noCrossVersioning:_*)
  )

  // Plugin hims
  lazy val playShimPlugin = (
    SbtShimPlugin("play", sbt12Version)
    dependsOn(sbtUiInterface)
    dependsOnRemote(playSbtPlugin12)
  )

  lazy val defaultsShimPlugin = (
    SbtShimPlugin("defaults", sbt12Version)
    // TODO - can we just depend on all the other plugins so we only have one shim?
  )

  // ================= END 0.12 shims ==========================


  // ================= 0.13 shims ==========================

  // Generic UI we use in all our shims and in the remote control to execute SBT as a UI.
  lazy val sbtUiInterface13 = (
      SbtShimPlugin("ui-interface", sbt13Version)
      settings(noCrossVersioning:_*)
      dependsOnSource("commons/ui-interface")
  )

  // This is the embedded controller for sbt projects.
  lazy val sbtControllerProbe13 = (
    SbtProbeProject("probe", sbt13Version)
    dependsOnSource("commons/protocol")
    dependsOnSource("commons/probe")
    dependsOnRemote(
      sbtControllerDeps(sbt13Version):_*
    )
    dependsOn(props, sbtUiInterface13 % "provided")
    settings(noCrossVersioning:_*)
  )

  // ================= Remote Controler main project ==========================


  val verboseSbtTests = Option(sys.props("sbtrc.verbose.tests")).map(_ == "true").getOrElse(false)

  def configureSbtTest(testKey: Scoped) = Seq(
    // set up embedded sbt for tests, we fork so we can set
    // system properties.
    Keys.fork in testKey := true,
    Keys.javaOptions in testKey <<= (
      SbtSupport.sbtLaunchJar,
      Keys.javaOptions in testKey,
      requiredClasspath in sbtControllerProbe,
      Keys.compile in Compile in sbtControllerProbe) map {
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
  lazy val sbtRemoteController = (
    SbtRemoteControlProject("remote-controller")
    settings(Keys.libraryDependencies <+= (Keys.scalaVersion) { v => "org.scala-lang" % "scala-reflect" % v })
    settings(
      Keys.publishArtifact in (Test, Keys.packageBin) := true 
    )
    dependsOnSource("commons/protocol")
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
    dependsOn(sbtRemoteController, props)
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
      // Additional dependencies required to run tests (so we don't re-resolve them):
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.10.1",
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.10.2",
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.9.2",
      localRepoArtifacts += "com.typesafe.play" % "play_2.10" % "2.2.0-RC1",
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.play" % "sbt-plugin" % "2.2.0-RC1", "0.13", "2.10"),
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.sbt" % "sbt-atmos" % "0.3.0-RC1", "0.13", "2.10"),
      // TODO - does this exist?
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.sbt" % "sbt-atmos" % "0.3.0-RC1", "0.12", "2.9.2"),
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0", "0.13", "2.10"),
      localRepoArtifacts += Defaults.sbtPluginExtra("com.github.mpeltonen" % "sbt-idea" % "1.5.1", "0.13", "2.10"),
      localRepoArtifacts += "com.novocode" % "junit-interface" % "0.7" % "test",
      Keys.resolvers += Resolver.url("typesafe-ivy-releases-2", new URL("http://private-repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)
    )
  )
  
}
