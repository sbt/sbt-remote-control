import sbt._
import Keys._

// Defines how to generate properties file based on build attributes.
object Properties {

  val makePropertiesSource = TaskKey[Seq[File]]("make-properties-source")

  def writeIfChanged(file: java.io.File, content: String): Unit = {
    val oldContent = if (file.exists) IO.read(file) else ""
    if (oldContent != content) {
      IO.write(file, content)
    }
  }

  def makePropertyClassSetting(sbtVersion: String, scalaVersion: String): Seq[Setting[_]] = Seq(
    resourceGenerators in Compile <+= makePropertiesSource,
    makePropertiesSource <<= (version, resourceManaged in Compile, compile in Compile) map { (v, dir, analysis) =>
      val parent= dir / "com/typesafe/sbtrc/properties"
      IO createDirectory parent
      val target = parent / "sbtrc.properties"

      writeIfChanged(target, makeJavaPropertiesString(v, sbtVersion, scalaVersion))

      Seq(target)
    }
  )

  
  def lastCompilationTime(analysis: sbt.inc.Analysis): Long = {
    val times = analysis.apis.internal.values map (_.compilation.startTime)
    if(times.isEmpty) 0L else times.max
  }
  

  def makeJavaPropertiesString(version: String, sbtVersion: String, scalaVersion: String): String =
    """|app.version=%s
       |sbt.version=%s
       |sbt.scala.version=%s
       |app.scala.version=%s
       |sbt.Xmx=512M
       |sbt.PermSize=128M
       |sbt.echo.default.version=%s
       |""".stripMargin format (version, sbtVersion, sbtScalaVersion(sbtVersion), scalaVersion, Dependencies.sbtEchoDefaultVersion)
  
  
  def sbtScalaVersion(sbtVersion: String): String =
    (sbtVersion split "[\\.\\-]" take 3) match {
      case Array("0", "12", _) => "2.9.2"
      case Array("0", "13", _) => "2.10.2"
      case _                   => "2.9.1"
    }
  
}
