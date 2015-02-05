import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

object Dependencies {

  def crossSbtVersion(sbtVersion: String): String =
    if(sbtVersion startsWith "0.13") "0.13"
    else if(sbtVersion startsWith "0.12") "0.12"
    else sys.error("Unsupported sbt version: " + sbtVersion)
  // Helpers for sbt plugins
  def getScalaVersionForSbtVersion(sbt: String) =
    crossSbtVersion(sbt) match {
      case "0.12" => "2.9.2"
      case "0.13" => "2.10.4"
      case _ => "2.10.4"
      //case _ => sys.error("Unsupported sbt version: " + sbt)
    }

  def makeSbtPlugin(m: ModuleID, sbtVersion: String): ModuleID = {
    val sbt: String = crossSbtVersion(sbtVersion)
    val fullScala: String = getScalaVersionForSbtVersion(sbtVersion)
    val scala: String = CrossVersion.binaryScalaVersion(fullScala)
    Defaults.sbtPluginExtra(m, sbt, scala)
  }

  // Reference versions
  val sbt13Version = "0.13.8-M3"
  val sbt13ScalaVersion = getScalaVersionForSbtVersion(sbt13Version)
  val sbtAtmosDefaultVersion = "0.3.1"

  // Here are the versions used for the core project
  val scalaVersion = "2.10.4"
  val scala211Version = "2.11.5"
  val sbtMainVersion = sbt13Version
  val playVersion = "2.3.4"
  val scalaCheckVersion = "1.11.5"
  val akkaVersion = "2.3.8"

  // Here we declare our dependencies normally
  val sbtOrg               = "org.scala-sbt"
  val sbtIo                = sbtOrg % "io" % sbtMainVersion
  val sbtCollections       = sbtOrg % "collections" % sbtMainVersion
  // We use an old version here, so we're compatible...
  val sbtLauncherInterface = sbtOrg % "launcher-interface" % sbtMainVersion
  val sbtCompilerInterface = sbtOrg % "interface" % sbtMainVersion
  val sbtCompletion        = sbtOrg % "completion" % sbtMainVersion

  val coreNextVersion      = "0.1.0-M3"
  val coreNext             = sbtOrg %% "core-next" % coreNextVersion
  val coreNext210          = sbtOrg % "core-next_2.10" % coreNextVersion
  val coreNextPlugin13     = Defaults.sbtPluginExtra(sbtOrg % "sbt-core-next" % coreNextVersion, "0.13", "2.10")

  val serializationVersion = "0.1.0-M2"
  val serialization210 = "org.scala-sbt" % "serialization_2.10" % serializationVersion
  val serialization211 = "org.scala-sbt" % "serialization_2.11" % serializationVersion
  val serialization = "org.scala-sbt" %% "serialization" % serializationVersion

  // These are needed for integration tests. Use the values used by sbt/serialization
  val picklingVersion = "0.10.0-M4"
  val pickling210 = "org.scala-lang.modules" % "scala-pickling_2.10" % picklingVersion
  val pickling211 = "org.scala-lang.modules" % "scala-pickling_2.11" % picklingVersion
  val pickling = "org.scala-lang.modules" %% "scala-pickling" % picklingVersion

  // These are needed for integration tests. Use the values used by sbt/serialization
  private val jsonTuples = Seq(
    ("org.json4s", "json4s-core", "3.2.10"),
    ("org.spire-math", "jawn-parser", "0.6.0"),
    ("org.spire-math", "json4s-support", "0.6.0")
  )
  val jsonDependencies = jsonTuples map {
    case (group, mod, version) => (group %% mod % version).exclude("org.scala-lang", "scalap")
  }
  val jsonDependencies210 = jsonTuples map {
    case (group, mod, version) => group % s"${mod}_2.10" % version
  }
  val jsonDependencies211 = jsonTuples map {
    case (group, mod, version) => group % s"${mod}_2.11" % version
  }

  val akkaTypsafe          = "com.typesafe.akka"
  val akkaActor            = akkaTypsafe %% "akka-actor" % akkaVersion
  val akkaTestkit          = akkaTypsafe %% "akka-testkit" % akkaVersion

  val jansi                = "org.fusesource.jansi" % "jansi" % "1.11"

  val commonsIo            = "commons-io" % "commons-io" % "2.0.1"

  val mimeUtil             = "eu.medsea.mimeutil" % "mime-util" % "2.1.1"
  // need to manually set this to override an incompatible old version
  val slf4jLog4j           = "org.slf4j" % "slf4j-log4j12" % "1.6.6"

  val junitInterface       = "com.novocode" % "junit-interface" % "0.11"
  val scalaCheck           = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  val specs2               = "org.specs2" %% "specs2" % "2.3.11"



  // Here we define dependencies for the shim/probe sections.
  def sbtControllerDeps(sbtVersion: String, provided: Boolean = true): Seq[ModuleID] = {
    if(provided) Seq(
      sbtOrg % "sbt" % sbtVersion % Provided.name
    ) else Seq(
      sbtOrg % "sbt" % sbtVersion
    )
  }


  // Mini DSL
  // DSL for adding remote deps like local deps.
  implicit def p2remote(p: Project): RemoteDepHelper = new RemoteDepHelper(p)
  final class RemoteDepHelper(p: Project) {
    def dependsOnRemote(ms: ModuleID*): Project = p.settings(libraryDependencies ++= ms)
  }
  // DSL for adding source dependencies to projects.
  def dependsOnSource(dir: String): Seq[Setting[_]] = {
    import Keys._
    Seq(unmanagedSourceDirectories in Compile <<= (unmanagedSourceDirectories in Compile, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir / "src/main/scala") +: srcDirs },
        unmanagedSourceDirectories in Test <<= (unmanagedSourceDirectories in Test, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir / "src/test/scala") +: srcDirs },
        sourceDirectories in ScalariformKeys.format in Compile <++= (unmanagedSourceDirectories in Compile, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir / "src/main/scala") +: srcDirs },
        sourceDirectories in ScalariformKeys.format in Test <++= (unmanagedSourceDirectories in Test, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir / "src/test/scala") +: srcDirs })
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
