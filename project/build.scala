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
  lazy val publishedProjects: Seq[Project] = Seq(client, client211, terminal, clientAll, clientAll211) ++ sbt13ProbeProjects


  // ================= 0.13 projects ==========================

  // Adapter UI interface for existing projects to pull in now.
  lazy val sbtUiInterface13 = (
      SbtShimPlugin("ui-interface", sbt13Version)
      dependsOnSource("commons/ui-interface")
      dependsOnRemote(playJson, brokenJodaRaw)
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
    settings(
      Keys.resourceGenerators in Compile += (Def.task {
        Properties.makeDefaultSbtVersionFile(sbt13Version, (Keys.resourceManaged in Compile).value)
      }).taskValue
    )
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

  def makeClientProject(name: String, scalaVersion: String): Project = {
    val crossVersionedSbtLibs = Seq(
      sbtIo,
      sbtCollections
    )
    SbtRemoteControlProject(name).
    settings(
       Keys.scalaVersion := scalaVersion,
       Keys.publishArtifact in (Test, Keys.packageBin) := true,
       resourceGenerators in Compile <+= 
         makeSbtLaunchProperties(
          "sbt-server.properties", 
          "com.typesafe.sbtrc.server.SbtServerMain", 
          Some(sbtServer13),
          Some("${user.dir}/project/.sbtserver")),
       resourceGenerators in Compile += Def.task {
         Seq(SbtSupport.sbtLaunchJar.value)
       }.taskValue,
       Keys.libraryDependencies ++= 
         Seq(
               "org.scala-lang" % "scala-reflect" % Keys.scalaVersion.value,
               playJson,
               brokenJoda,
               sbtLauncherInterface,
               sbtCompilerInterface
         ),
       Keys.libraryDependencies ++= (Keys.scalaBinaryVersion.value match {
          case "2.10" => crossVersionedSbtLibs
          case "2.11" => crossVersionedSbtLibs map (_ cross CrossVersion.binary)
          case v => sys.error(s"Unsupported scalaBinary version: $v")
       })
    ).
    dependsOnSource("commons/protocol").
    dependsOnSource("client")
  }

  // We load the client code from the client directory and build both a Scala 2.10 and Scala 2.11 variant.
  lazy val client: Project = makeClientProject("client-2-10", Dependencies.scalaVersion)
  lazy val client211: Project = makeClientProject("client-2-11", Dependencies.scala211Version)

  // TODO - OSGi settings for this guy...
  def makeClientAllProject(name: String, scalaVersion: String, clientProj: Project): Project = (
    SbtRemoteControlRepackagedProject(name)
    dependsOn(clientProj % s"${RepackageDep.name}")
    settings(
      Keys.scalaVersion := scalaVersion,
      libraryDependencies += "org.scala-lang" % "scala-reflect" % Keys.scalaVersion.value,
      // TODO - maybe this belongs in the helper. It's needed because
      // the helper doesn't correctly pull in the local project dep, thanks to how
      // sbt hooks ivy.
      managedClasspath in RepackageDep += {
        Attributed.blank((packageBin in Compile in clientProj).value).put(Keys.moduleID.key, (Keys.projectID in clientProj).value)
      },
      // Remove all dependencies on any local project, only keep the scala ones.
      // TODO - Why is makePom pulling in configurations it can't handle?
      pomPostProcess := { pom: scala.xml.Node =>
        (new scala.xml.transform.RewriteRule {
          private def isBadDep(n: scala.xml.Node): Boolean =
            // TODO - Don't hardcode the org.
            ((n \ "groupId").text == "com.typesafe.sbtrc") ||
            ((n \ "artifactId").text == "junit-interface")
          override def transform(n: scala.xml.Node): Seq[scala.xml.Node] =
            n match {
              case dep: scala.xml.Elem if isBadDep(dep) => Nil
              case elem: scala.xml.Elem => elem copy (child = elem.child flatMap (this transform))
              case other => other
            }
        } transform pom).head
      }
    )
  )
  lazy val clientAll = makeClientAllProject("client-all", Dependencies.scalaVersion, client)
  lazy val clientAll211 = makeClientAllProject("client-all-2-11", Dependencies.scala211Version, client211)


  lazy val terminal: Project = (
    SbtRemoteControlProject("terminal")
    dependsOn(client)
    dependsOnRemote(sbtCompletion)
    settings(
      resourceGenerators in Compile <+= makeSbtLaunchProperties("sbt-client.properties", "com.typesafe.sbtrc.client.SimpleSbtTerminal")
    )
  )

  // Set up a repository that has all our dependencies.
  import Project.Initialize

  // right now we publish this project because Activator
  // is using TestUtil; but we should probably clean it
  // up later so we only publish a test framework
  // and not the whole thing.
  lazy val itTests: Project = (
    SbtRemoteControlProject("integration-tests")
    dependsOn(clientAll211)
    settings(
      Keys.scalaVersion := Dependencies.scala211Version,
      //com.typesafe.sbtidea.SbtIdeaPlugin.ideaIgnoreModule := true
      // We have to expose the jar here, because the normal exportJars value
      // causes a circular task dependency with sbt-assembly
      Keys.managedClasspath in Compile += {
        Attributed.blank((packageBin in Compile in clientAll211).value)
      }
    )
  )

  lazy val itRunner: Project = (
    SbtRemoteControlProject("it-runner")
    settings(integration.settings(publishedProjects :+ itTests, itTests): _*)
    settings(
      Keys.scalaVersion := Dependencies.scala211Version,
      //com.typesafe.sbtidea.SbtIdeaPlugin.ideaIgnoreModule := true,
      Keys.publish := {},
      Keys.publishLocal := {},
      Keys.resolvers += Resolver.url("typesafe-ivy-private-releases", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
      // Additional dependencies required to run tests (so we don't re-resolve them):
      localRepoArtifacts += jansi,
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % Dependencies.scalaVersion,
      localRepoArtifacts += "org.scala-lang" % "scala-compiler" % Dependencies.scala211Version,
      // TODO - We should support the cross-versioning semantics of sbt when generating local artifact repositories...
      localRepoArtifacts += "com.typesafe.play" % "play-json_2.10" % Dependencies.playVersion,

      localRepoArtifacts += "org.scala-sbt" % "sbt" % Dependencies.sbt13Version,
      //localRepoArtifacts += "org.scala-sbt" % "io_2.11" % Dependencies.sbt13Version,
      //localRepoArtifacts += "org.scala-sbt" % "collections_2.11" % Dependencies.sbt13Version,
      //localRepoArtifacts += "com.typesafe.play" % "play-json_2.11" % Dependencies.playVersion,
      localRepoArtifacts += junitInterface % "test",
      Keys.resolvers += Resolver.url("typesafe-ivy-releases-2", new URL("http://private-repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)
    )
  )

}
