import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

object SbtRcBuild {
  val gitHeadCommit = SettingKey[Option[String]]("git-head-commit")
  val gitCurrentTags = SettingKey[Seq[String]]("git-current-tags")

  // TODO - Don't calculate versions EVERYWHERE, just in global...
  def baseVersions: Seq[Setting[_]] = Seq(
    gitHeadCommit <<= (baseDirectory) apply { bd =>
      jgit(bd).headCommit
    },
    gitCurrentTags <<= (baseDirectory) apply { bd =>
      // sort these descending
      jgit(bd).currentTags.sorted.reverse
    },
    version <<= (gitHeadCommit, gitCurrentTags) apply makeVersion
  )

  def makeVersion(headCommit: Option[String], currentTags: Seq[String]): String = {
    // TODO - move this into a setting somewhere....
    val baseVersion = "1.0"
    
    def releaseVersion: Option[String] = {
      val releaseVersions = 
        for {
          tag <- currentTags
          if tag matches "v[0-9].*"
        } yield tag drop 1
      releaseVersions.headOption
    }
    
    def commitVersion: Option[String] =
      headCommit map (sha => baseVersion + "-" + sha)
    
    def dateVersion: String = {
      val df = new java.text.SimpleDateFormat("yyyyMMdd'T'HHmmss")
      df setTimeZone java.util.TimeZone.getTimeZone("GMT")
      baseVersion + "-" + (df format (new java.util.Date))
    }

    def overrideVersion = Option(sys.props("activator.version")) 
    
	// Now we fall through the potential version numbers...
    overrideVersion  orElse releaseVersion orElse commitVersion getOrElse dateVersion 
  }
  
  def formatPrefs = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(IndentSpaces, 2)
  }

  val typesafeIvyReleases = Resolver.url("typesafe-ivy-private-releases", new URL("http://private-repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
  // TODO - When SBT 0.13 is out we won't need this...
  val typesafeIvySnapshots = Resolver.url("typesafe-ivy-private-snapshots", new URL("http://private-repo.typesafe.com/typesafe/ivy-snapshots/"))(Resolver.ivyStylePatterns)

  def sbtrcDefaults: Seq[Setting[_]] =
    SbtScalariform.scalariformSettings ++
    Seq(
      organization := "com.typesafe.sbtrc",
      version <<= version in ThisBuild,
      crossPaths := false,
      resolvers += "typesafe-mvn-releases" at "http://repo.typesafe.com/typesafe/releases/",
      resolvers += Resolver.url("typesafe-ivy-releases", new URL("http://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns),
      // TODO - This won't be needed when SBT 0.13 is released...
      resolvers += typesafeIvyReleases,
      resolvers += typesafeIvySnapshots,
      // TODO - Publish to ivy for sbt plugins, maven central otherwise?
      publishTo := Some(typesafeIvyReleases),
      publishMavenStyle := false,
      scalacOptions <<= (scalaVersion) map { sv =>
        Seq("-unchecked", "-deprecation") ++
          { if (sv.startsWith("2.9")) Seq.empty else Seq("-feature") }
      },
      javacOptions in Compile := Seq("-target", "1.6", "-source", "1.6"),
      javacOptions in (Compile, doc) := Seq("-source", "1.6"),
      libraryDependencies += Dependencies.junitInterface % "test",
      scalaVersion := Dependencies.scalaVersion,
      scalaBinaryVersion := "2.10",
      ScalariformKeys.preferences in Compile := formatPrefs,
      ScalariformKeys.preferences in Test    := formatPrefs
    )

  def sbtShimPluginSettings: Seq[Setting[_]] =
    sbtrcDefaults ++
    Seq(
      scalaVersion := Dependencies.sbtPluginScalaVersion,
      scalaBinaryVersion := Dependencies.sbtPluginScalaVersion,
      sbtPlugin := true,
      publishMavenStyle := false
    )

  def SbtRemoteControlProject(name: String): Project = (
    Project("sbt-rc-" + name, file("sbt-rc") / name)
    settings(sbtrcDefaults:_*)
  )

  def SbtShimPlugin(name: String): Project = (
    Project("sbt-shim-" + name, file("sbt-shim") / name)
    settings(sbtShimPluginSettings:_*)
  )
  def PropsProject(name: String): Project = (
    Project("sbt-rc-" + name, file(name))
    settings(sbtrcDefaults:_*)
    settings(
        autoScalaLibrary := false
    )
  )
}
