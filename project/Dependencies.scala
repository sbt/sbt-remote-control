import sbt._
import Keys._

object Dependencies {

  // Helpers for sbt plugins
  def getScalaVersionForSbtVersion(sbt: String) =
    CrossVersion.binarySbtVersion(sbt) match {
      case "0.12" => "2.9.2"
      case "0.13" => "2.10.4"
      case _ => sys.error("Unsupported sbt version: " + sbt)
    }

  def makeSbtPlugin(m: ModuleID, sbtVersion: String): ModuleID = {
    val sbt: String = CrossVersion.binarySbtVersion(sbtVersion)
    val fullScala: String = getScalaVersionForSbtVersion(sbtVersion)
    val scala: String = CrossVersion.binaryScalaVersion(fullScala)
    Defaults.sbtPluginExtra(m, sbt, scala)
  }

  // Reference versions
  val sbt12Version = "0.12.4"
  val sbt12ScalaVersion = getScalaVersionForSbtVersion(sbt12Version)
  val sbt13Version = "0.13.5"
  val sbt13ScalaVersion = getScalaVersionForSbtVersion(sbt13Version)
  // this version is used for sbtIo but NOT to actually run sbt
  val sbtLibraryVersion = "0.13.6-M1"

  // Make sure to update supported versions if Echo plugin version is changed
  val sbtEcho12Version = "0.1.1"
  val sbtEcho132Version = "0.1.1.3"
  val sbtEcho135Version = "0.1.5"
  val sbt132EchoSupportedAkkaVersion = "2.2.4"
  val sbt132EchoSupportedPlayVersion = "2.2.3"
  val sbt135EchoSupportedAkkaVersion = "2.3.4"
  val sbt135EchoSupportedPlayVersion = "2.3.3"

  // Here are the versions used for the core project
  val scalaVersion = "2.11.1"
  val akkaVersion = "2.3.4"

  // Here we declare our dependencies normally
  val sbtOrg               = "org.scala-sbt"
  val sbtIo                = sbtOrg %% "io" % sbtLibraryVersion
  val sbtLauncherInterface    = "org.scala-sbt" % "launcher-interface" % sbt13Version
  val sbtCompletion           = "org.scala-sbt" %% "completion" % sbtLibraryVersion
  val scalaParserCombinators  = "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.1"

  val akkaActor            = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaSlf4j            = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaTestkit          = "com.typesafe.akka" %% "akka-testkit" % akkaVersion

  val commonsIo            = "commons-io" % "commons-io" % "2.0.1"

  val mimeUtil             = "eu.medsea.mimeutil" % "mime-util" % "2.1.1"
  // need to manually set this to override an incompatible old version
  val slf4jLog4j           = "org.slf4j" % "slf4j-log4j12" % "1.6.6"

  val junitInterface       = "com.novocode" % "junit-interface" % "0.7"
  //val specs2               = "org.specs2" % "specs2_2.10" % "1.13"

  // Here we define dependencies for the shim/probe sections.
  def sbtControllerDeps(sbtVersion: String): Seq[ModuleID] = {
    Seq(
      sbtOrg % "sbt" % sbtVersion % Provided.name
    )
  }
  val playSbtPlugin12        =  makeSbtPlugin("play" % "sbt-plugin" % "2.1.5", sbt12Version)
  val eclipseSbtPlugin12     =  makeSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0", sbt12Version)
  val ideaSbtPlugin12        =  makeSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2", sbt12Version)


  // Mini DSL
  // DSL for adding remote deps like local deps.
  implicit def p2remote(p: Project): RemoteDepHelper = new RemoteDepHelper(p)
  final class RemoteDepHelper(p: Project) {
    def dependsOnRemote(ms: ModuleID*): Project = p.settings(libraryDependencies ++= ms)
  }
  // DSL for adding source dependencies ot projects.
  def dependsOnSource(dir: String): Seq[Setting[_]] = {
    import Keys._
    Seq(unmanagedSourceDirectories in Compile <<= (unmanagedSourceDirectories in Compile, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir / "src/main/scala") +: srcDirs },
        unmanagedSourceDirectories in Test <<= (unmanagedSourceDirectories in Test, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir / "src/test/scala") +: srcDirs })
  }
  implicit def p2source(p: Project): SourceDepHelper = new SourceDepHelper(p)
  final class SourceDepHelper(p: Project) {
    def dependsOnSource(dir: String): Project =
      p.settings(Dependencies.dependsOnSource(dir):_*)
  }

  // compile classpath and classes directory, with provided/optional or scala dependencies
  // specifically for projects that need remote-probe dependencies
  val requiredClasspath = TaskKey[Classpath]("required-classpath")

  def requiredJars(deps: ProjectReference*): Setting[_] = {
    import xsbti.ArtifactInfo._
    import Project.Initialize
    val dependentProjectClassPaths: Seq[Initialize[Task[Seq[File]]]] =
      (deps map { proj =>
        (classDirectory in Compile in proj) map { dir => Seq(dir) }
      })
    val ivyDeps: Initialize[Task[Seq[File]]] =  update map { report =>
      val jars = report.matching(configurationFilter(name = "compile") -- moduleFilter(organization = ScalaOrganization, name = ScalaLibraryID))
      jars
    }
    val localClasses: Initialize[Task[Seq[File]]] = (classDirectory in Compile) map { dir =>
      Seq(dir)
    }
    // JOin everyone
    def joinCp(inits: Seq[Initialize[Task[Seq[File]]]]): Initialize[Task[Seq[File]]] =
      inits reduce { (lhs, rhs) =>
        (lhs zip rhs).flatMap { case (l,r) =>
          l.flatMap[Seq[File]] { files =>
            r.map[Seq[File]] { files2 =>
              files ++ files2
            }
          }
        }
      }
    requiredClasspath <<= joinCp(dependentProjectClassPaths ++ Seq(ivyDeps, localClasses)) map {
      _.classpath
    }
  }
}
