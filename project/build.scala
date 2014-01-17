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
  lazy val sbt13ProbeProjects = Set(sbtUiInterface13, sbtServer13)
  lazy val publishedProjects: Seq[Project] = Seq(sbtRemoteController) ++ sbt13ProbeProjects



  // ================= 0.13 projects ==========================

  // Adapter UI interface for existing projects to pull in now.
  lazy val sbtUiInterface13 = (
      SbtShimPlugin("ui-interface", sbt13Version)
      dependsOnSource("commons/ui-interface")
      dependsOnRemote(playJson)
  )
  // Wrapper around sbt 0.13.x that runs as a server.
  lazy val sbtServer13 = (
    SbtProbeProject("server", sbt13Version)
    dependsOnSource("commons/protocol")
    dependsOnSource("commons/ui-interface")
    dependsOnRemote(
      sbtControllerDeps(sbt13Version, provided=false):_*
    )
    dependsOnRemote(playJson, brokenJoda)
    settings(noCrossVersioning:_*)
  )

  // ================= Remote Controler main project ==========================


  import Keys._
  import Project.Initialize
  def makeSbtLaunchProperties(filename: String, mainClass: String, project: Option[Project] = None, lockFile: Option[String] = None): Initialize[Task[Seq[File]]] =
    (resourceManaged in Compile, project.map(r => projectID in r).getOrElse(projectID)) map { (target, id) =>
        val file = target / filename
        val contents = """|[scala]
                    |  version: ${sbt.scala.version-auto}
                    |[app]
                    |  org: %s
                    |  name: %s
                    |  version: %s
                    |  class: %s
                    |  components: xsbti,extra
                    |  cross-versioned: ${sbt.cross.versioned-false}
                    |  resources: ${sbt.extraClasspath-}
                    |
                    |[repositories]
                    |  local
                    |  maven-central
                    |
                    |[boot]
                    |  directory: ${sbt.boot.directory-${sbt.global.base-${user.home}/.sbt}/boot/}
                    |
                    |[ivy]
                    |  ivy-home: ${sbt.ivy.home-${user.home}/.ivy2/}
                    |  checksums: ${sbt.checksums-sha1,md5}
                    |  override-build-repos: ${sbt.override.build.repos-false}
                    |  repository-config: ${sbt.repository.config-${sbt.global.base-${user.home}/.sbt}/repositories}
                    |
                    |""".stripMargin.format(id.organization, id.name, id.revision, mainClass)
       val fullContents = lockFile match {
         case Some(file) => contents + """|[server]
                                          |  lock: %s
                                          |  jvmargs: ${sbt.jvmargs-${sbt.global.base-${user.home}/.sbt}/.jvmargs}
                                          |  jvmprops: ${sbt.jvmprops-${sbt.global.base-${user.home}/.sbt}/.jvmprops}""".stripMargin.format(file)
         case None => contents
       }
       IO.write(file, fullContents)
       Seq(file)
    }

  // This project is used to drive sbt processes, installing the controller.
  lazy val sbtRemoteController: Project = (
    SbtRemoteControlProject("client")
    settings(Keys.libraryDependencies <+= (Keys.scalaVersion) { v => "org.scala-lang" % "scala-reflect" % v })
    settings(
      Keys.publishArtifact in (Test, Keys.packageBin) := true,
      resourceGenerators in Compile <+= makeSbtLaunchProperties("sbt-server.properties", "com.typesafe.sbtrc.server.SbtServerMain", Some(sbtServer13), Some("${user.dir}/project/.sbtserver")),
      resourceGenerators in Compile <+= makeSbtLaunchProperties("sbt-client.properties", "com.typesafe.sbtrc.client.SimpleSbtTerminal")
    )
    dependsOnSource("commons/protocol")
    dependsOnRemote(playJson, brokenJoda)
    dependsOnRemote(sbtLauncherInterface,
                    sbtCompilerInterface,
                    sbtIo, sbtCollections)
  )

  // Set up a repository that has all our dependencies.
  import Project.Initialize

  lazy val itTests: Project = (
    SbtRemoteControlProject("integration-tests")
    dependsOnRemote(sbtLauncherInterface, sbtIo)
    dependsOn(sbtRemoteController)
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
      Keys.resolvers += Resolver.url("typesafe-ivy-private-releases", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
      // Additional dependencies required to run tests (so we don't re-resolve them):
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.10.1",
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.10.2",
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % "2.9.2",
      localRepoArtifacts += "com.typesafe.play" % "play_2.10" % "2.2.0",
      localRepoArtifacts += "play" % "play_2.10" % "2.1.5",
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.play" % "sbt-plugin" % "2.2.0", "0.13", "2.10"),
      localRepoArtifacts += Dependencies.playSbtPlugin12,
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.sbt" % "sbt-atmos" % "0.3.1", "0.13", "2.10"),
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.sbt" % "sbt-atmos" % "0.3.1", "0.12", "2.9.2"),
      localRepoArtifacts += Defaults.sbtPluginExtra("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0", "0.13", "2.10"),
      localRepoArtifacts += Defaults.sbtPluginExtra("com.github.mpeltonen" % "sbt-idea" % "1.5.2", "0.13", "2.10"),
      localRepoArtifacts += "com.novocode" % "junit-interface" % "0.7" % "test",
      Keys.resolvers += Resolver.url("typesafe-ivy-releases-2", new URL("http://private-repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)
    )
  )

}
