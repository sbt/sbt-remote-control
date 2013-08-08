package com.typesafe.sbtrc

/** Helpers to map from sbt types into serializable json types. */
object SbtToProtocolUtils {
  
  
  def manifestToProtocol[T](mf: Manifest[T]): protocol.TypeInfo = {
    protocol.TypeInfo(
      mf.erasure.getName,
      mf.typeArguments map (x => manifestToProtocol(x))
    )
  }
  
  def keyToProtocol[T](key: sbt.AttributeKey[T]): protocol.AttributeKey =
    protocol.AttributeKey(
      key.label,
      manifestToProtocol(key.manifest)
    )
    
    
  def scopeToProtocol(scope: sbt.Scope): protocol.SbtScope = {
    // TODO - Figure this out
    val configString: Option[String] = 
      scope.config.toOption.map(_.name)  // TODO - this is not right
    
    val projectString: Option[protocol.ProjectReference] =
      scope.project.toOption.map(referenceToProtocolProject) // TODO - better project name!

    val taskString: Option[protocol.AttributeKey] =
      scope.task.toOption.map(x => keyToProtocol(x))
      
    // TODO - Build.... (probably from project reference)...
    protocol.SbtScope(
        config = configString,
        project = projectString,
        task = taskString)
  }
  
  
  def scopedKeyToProtocol[T](key: sbt.ScopedKey[T]): protocol.ScopedKey =
    protocol.ScopedKey(
      key = keyToProtocol(key.key),
      scope = scopeToProtocol(key.scope)
    )
  
 
    // TODO - This needs some work.
  def referenceToProtocolProject(ref: sbt.Reference): protocol.ProjectReference = {
    import sbt._
    ref match {
      case x: ProjectRef => 
        protocol.ProjectReference(x.build, x.project)
      case _ => throw new RuntimeException("was expected project refrence, got: " + ref)
    }
  }
    
}