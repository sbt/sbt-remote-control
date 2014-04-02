import sbt._
import Keys._

object Dependencies {

  def crossSbtVersion(sbtVersion: String): String =
    if(sbtVersion startsWith "0.13") "0.13"
    else if(sbtVersion startsWith "0.12") "0.12"
    else sys.error("Unsupported sbt version: " + sbtVersion)
  // Helpers for sbt plugins
  def getScalaVersionForSbtVersion(sbt: String) =
    crossSbtVersion(sbt) match {
      case "0.12" => "2.9.2"
      case "0.13" => "2.10.3"
      case _ => "2.10.3"
      case _ => sys.error("Unsupported sbt version: " + sbt)
    }

  def makeSbtPlugin(m: ModuleID, sbtVersion: String): ModuleID = {
    val sbt: String = crossSbtVersion(sbtVersion)
    val fullScala: String = getScalaVersionForSbtVersion(sbtVersion)
    val scala: String = CrossVersion.binaryScalaVersion(fullScala)
    Defaults.sbtPluginExtra(m, sbt, scala)
  }

  // Reference versions
  val sbt12Version = "0.12.4"
  val sbt12ScalaVersion = getScalaVersionForSbtVersion(sbt12Version)
  val sbt13Version = "0.13.5-M2"
  val sbt13ScalaVersion = getScalaVersionForSbtVersion(sbt13Version)
  val sbtAtmosDefaultVersion = "0.3.1"

  // Here are the versions used for the core project
  val scalaVersion = "2.10.3"
  val sbtMainVersion = sbt13Version
  val akkaVersion = "2.2.0"
  val playVersion = "2.2.1"


  // Here we declare our dependencies normally
  val sbtOrg               = "org.scala-sbt"
  val sbtIo                = sbtOrg % "io" % sbtMainVersion
  val sbtCollections       = sbtOrg % "collections" % sbtMainVersion
  // We use an old version here, so we're compatible...
  val sbtLauncherInterface = sbtOrg % "launcher-interface" % sbtMainVersion
  val sbtCompilerInterface = sbtOrg % "interface" % sbtMainVersion
  val sbtCompletion        = sbtOrg % "completion" % sbtMainVersion

  val playJson             = ("com.typesafe.play" % "play-json_2.10" % playVersion).exclude("com.typesafe", "play-iteratees_2.10").exclude("org.joda", "joda-time").exclude("org.joda", "joda-convert")
  val brokenJoda           = "org.joda" % "joda-convert" % "1.2" % "provided"
  val akkaActor            = "com.typesafe.akka" % "akka-actor_2.10" % akkaVersion
  val akkaSlf4j            = "com.typesafe.akka" % "akka-slf4j_2.10" % akkaVersion
  val akkaTestkit          = "com.typesafe.akka" % "akka-testkit_2.10" % akkaVersion

  val commonsIo            = "commons-io" % "commons-io" % "2.0.1"

  val mimeUtil             = "eu.medsea.mimeutil" % "mime-util" % "2.1.1"
  // need to manually set this to override an incompatible old version
  val slf4jLog4j           = "org.slf4j" % "slf4j-log4j12" % "1.6.6"

  val junitInterface       = "com.novocode" % "junit-interface" % "0.7"
  //val specs2               = "org.specs2" % "specs2_2.10" % "1.13"



  // Here we define dependencies for the shim/probe sections.
  def sbtControllerDeps(sbtVersion: String, provided: Boolean = true): Seq[ModuleID] = {
    if(provided) Seq(
      sbtOrg % "sbt" % sbtVersion % Provided.name
    ) else Seq(
      sbtOrg % "sbt" % sbtVersion
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
