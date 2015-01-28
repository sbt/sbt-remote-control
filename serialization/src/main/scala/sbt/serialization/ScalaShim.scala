package sbt.serialization

// Note this is only used by ManifestUtil.scala, and can be removed if that is removed.
private[serialization] object ScalaShims {
  val ManifestFactory = scala.reflect.ManifestFactory
}
