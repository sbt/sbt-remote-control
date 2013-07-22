import sbt._
import SbtRcBuild._
import Dependencies._
// NOTE - This file is only used for SBT 0.12.x, in 0.13.x we'll use build.sbt and scala libraries.
// As such try to avoid putting stuff in here so we can see how good build.sbt is without build.scala.


object TheBuild extends Build {

  // ADD sbt launcher support here.
  override def settings = super.settings ++ SbtSupport.buildSettings ++ baseVersions

  val root = (
    Project("root", file("."))  // TODO - Oddities with clean..
    aggregate((publishedProjects.map(_.project) ++ Seq(it.project)):_*)
    settings(
      // Stub out commands we run frequently but don't want them to really do anything.
      Keys.publish := {},
      Keys.publishLocal := {}
    )
  )

  // These are the projects we want in the local repository we deploy.
  lazy val publishedSbtShimProjects = Set(playShimPlugin, eclipseShimPlugin, ideaShimPlugin, sbtUiInterface, defaultsShimPlugin)
  lazy val publishedProjects: Seq[Project] = Seq(sbtRemoteController, sbtDriver, props) ++ publishedSbtShimProjects


  // A hack project just for convenient IvySBT when resolving artifacts into new local repositories.
  lazy val dontusemeresolvers = (
    Project("dontuseme", file("dontuseme"))
    settings(
      // This hack removes the project resolver so we don't resolve stub artifacts.
      Keys.fullResolvers <<= (Keys.externalResolvers, Keys.sbtResolver) map (_ :+ _),
      Keys.resolvers += Resolver.url("sbt-plugin-releases", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
      Keys.publish := {},
      Keys.publishLocal := {}
    )
  )
  
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

  import IvyRepositories.localRepoArtifacts
  import IvyRepositories.localRepoProjectsPublished
  lazy val it = (
    SbtRemoteControlProject("integration-tests")
    settings(integration.settings(dontusemeresolvers): _*)
    dependsOnRemote(sbtLauncherInterface, sbtIo)
    dependsOn(sbtDriver, props)
    settings(
      //com.typesafe.sbtidea.SbtIdeaPlugin.ideaIgnoreModule := true,
      Keys.publish := {},
      localRepoProjectsPublished <<= (publishedProjects map (Keys.publishLocal in _)).dependOn,
      localRepoProjectsPublished <<= (localRepoProjectsPublished, Keys.publishLocal) map { (_,_) => () },
      localRepoArtifacts <++= (publishedProjects filterNot publishedSbtShimProjects map { ref =>
        // The annoyance caused by cross-versioning.
        (Keys.projectID in ref, Keys.scalaBinaryVersion in ref, Keys.scalaVersion in ref) apply {
          (id, sbv, sv) =>
            CrossVersion(sbv, sv)(id)
        }
      }).join,
      localRepoArtifacts <++= (publishedSbtShimProjects.toSeq map { ref =>
        (Keys.projectID in ref) apply { id =>
          Defaults.sbtPluginExtra(id, sbtPluginVersion, sbtPluginScalaVersion)
        }
      }).join,
      localRepoArtifacts ++= Seq(
          // We need sbt itself to run our tests, here are the dependnecies.
          "org.scala-sbt" % "sbt" % Dependencies.sbtVersion,
          // For some reason, these are not resolving transitively correctly!
          "org.scala-lang" % "scala-compiler" % Dependencies.sbtPluginScalaVersion,
          "org.scala-lang" % "scala-compiler" % Dependencies.scalaVersion,
          "net.java.dev.jna" % "jna" % "3.2.3")
    )
  )
  
}
