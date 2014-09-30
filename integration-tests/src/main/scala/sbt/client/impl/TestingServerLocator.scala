package sbt.client.impl

import sbt.client._
import java.io.File
import java.net.URL
import xsbti.{
  AppConfiguration,
  PredefinedRepository,
  MavenRepository,
  IvyRepository
}
import java.nio.charset.Charset.defaultCharset

object Testing {
  def connector(configuration: xsbti.AppConfiguration, projectDirectory: java.io.File): SbtConnector =
    new SimpleConnector("sbt-client-test", "SbtClientTest unit test",
      projectDirectory, locator(configuration, new File(projectDirectory, "../sbt-global")))

  // TODO - Create a prop-file locator that uses our own repositories to
  // find the classes, so we use cached values...
  def locator(configuration: xsbti.AppConfiguration, globalDir: File): LaunchedSbtServerLocator = new LaunchedSbtServerLocator {
    // TODO - Do we care if the version for this directory is different?
    def sbtProperties(directory: File): URL =
      rewrittenPropsUrl

    lazy val propsFile: File = {
      val tmp = java.io.File.createTempFile("sbt-server", "properties")
      tmp.deleteOnExit()
      sbt.IO.write(tmp, s"sbt.global.base=${globalDir.toString}")
      tmp
    }

    // Rewrites boot properties for debugging.
    lazy val rewrittenPropsUrl: URL = {
      val tmp = java.io.File.createTempFile("sbt-server", "properties")
      val existing = getClass.getClassLoader.getResource("sbt-server.properties")
      val oldLines = sbt.IO.readLinesURL(existing, defaultCharset)
      val newLines = makeNewLaunchProperties(oldLines)
      sbt.IO.writeLines(tmp, newLines, defaultCharset, false)
      tmp.deleteOnExit()
      tmp.toURI.toURL
    }

    private def repositories: List[String] = {
      configuration.provider.scalaProvider.launcher.ivyRepositories.toList map {
        case ivy: IvyRepository =>
          // TODO - We only support converting things we care about here, not unviersally ok.
          val pattern = Option(ivy.ivyPattern).map(",".+).getOrElse("")
          val aPattern = Option(ivy.artifactPattern).map(",".+).getOrElse("")
          val mvnCompat = if (ivy.mavenCompatible) ", mavenCompatible" else ""
          "  " + ivy.id + ": " + ivy.url + pattern + aPattern + mvnCompat
        case mvn: MavenRepository => "  " + mvn.id + ": " + mvn.url
        case predef: PredefinedRepository => "  " + predef.id.toString
      }
    }

    // TODO - We also need to rewrite the line about the boot directory to use our boot directory.
    private def makeNewLaunchProperties(old: List[String]): List[String] = {
      val header = old.takeWhile(line => !line.contains("[repositories]"))
      val tail = old.dropWhile(line => !line.contains("[boot]")).map {
        // TODO - did we get all the overrides we need?
        case x if x contains "directory:" => s"  directory: ${configuration.provider.scalaProvider.launcher.bootDirectory.toString}"
        case x if x contains "ivy-home:" => s"  ivy-home: ${configuration.provider.scalaProvider.launcher.ivyHome.toString}"
        case x if x contains "override-build-repos:" => "override-build-repos: true"
        case x if x contains "repository-config:" => ""
        case x if x contains "jvmprops:" => s"  jvmprops: ${propsFile.toString}"
        case x => x
      }
      header ++ ("[repositories]" :: repositories) ++ List("") ++ tail
    }
  }
}
