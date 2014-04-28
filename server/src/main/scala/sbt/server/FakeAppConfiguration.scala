package sbt
package server

import xsbti._

object FakeAppConfiguration {
  // Reads the build-configured lowest-level sbt version
  def defaultSbtVersion = {
    val props = new java.util.Properties
    val input = getClass.getClassLoader.getResourceAsStream("default.sbt.version")
    try props load input
    finally {
      input.close()
    }
    Option(props.getProperty("sbt.version")) getOrElse
      sys.error("Unable to read default sbt properties file. This could be a bad bundle of sbt-remote-control.")
  }
}
// TODO - make this less hacky
case class FakeAppConfiguration(original: AppConfiguration, sbtVersion: String = FakeAppConfiguration.defaultSbtVersion) extends AppConfiguration {
  final val arguments: Array[String] = Array.empty
  final def baseDirectory: File = original.baseDirectory
  private def origAp = original.provider
  object provider extends xsbti.AppProvider {
    override def scalaProvider = origAp.scalaProvider
    object id extends ApplicationID {
      override val groupID = "org.scala-sbt"
      override val name = "sbt"
      override val version = sbtVersion
      override val mainClass = "sbt.xMain"
      override val mainComponents = origAp.id.mainComponents
      override val crossVersioned = origAp.id.crossVersioned
      override val crossVersionedValue = origAp.id.crossVersionedValue
      override val classpathExtra = origAp.id.classpathExtra
    }
    override def loader: ClassLoader = origAp.loader
    override def mainClass: Class[T] forSome { type T <: AppMain } = origAp.mainClass
    override def entryPoint: Class[_] = origAp.entryPoint
    override def newMain: AppMain = origAp.newMain
    override def mainClasspath: Array[File] = origAp.mainClasspath
    override def components: ComponentProvider = origAp.components
  }
}