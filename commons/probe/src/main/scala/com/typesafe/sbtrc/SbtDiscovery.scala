package com.typesafe.sbtrc

import _root_.sbt._
import SbtToProtocolUtils._

// This is a helper class that lets us run discovery methods on sbt.
object SbtDiscovery {
  
  // have to leave the type inferencer here.
  def structure(state: State) =
    Project.extract(state).structure
  
  def keyIndex(state: State): sbt.KeyIndex =
    structure(state).index.keyIndex
    
  def builds(state: State): Set[String] =
    keyIndex(state).buildURIs map (_.toASCIIString)
  
  def projects(state: State, build: URI): Set[protocol.ProjectReference] = 
    keyIndex(state).projects(build) map { name =>
      protocol.ProjectReference(build, name)  
    }

  def keys(state: State, filter: protocol.KeyFilter): Seq[protocol.ScopedKey] =
    filteredkeys(state, filter).map(x => scopedKeyToProtocol(x))
  
  def tasks(state: State, filter: protocol.KeyFilter): Seq[protocol.ScopedKey] =
    for {
      key <- filteredkeys(state, filter)
      if isTaskKey(key)
    } yield scopedKeyToProtocol(key)
  
  def inputTasks(state: State, filter: protocol.KeyFilter): Seq[protocol.ScopedKey] =
    for {
      key <- filteredkeys(state, filter)
      if isInputKey(key)
    } yield scopedKeyToProtocol(key)
  
  // Settings must not be tasks or inputTasks.
  def settings(state: State, filter: protocol.KeyFilter): Seq[protocol.ScopedKey] =
    for {
      key <- filteredkeys(state, filter)
      if !isInputKey(key)
      if !isTaskKey(key)
    } yield scopedKeyToProtocol(key)
    
    
  private def filteredkeys(state: State, filter: protocol.KeyFilter): Seq[sbt.ScopedKey[_]] = {
    // TODO - Figure out filtering...
    for {
      setting <- structure(state).settings
      key = setting.key
      if shouldIncludeKey(filter)(key)
    } yield key
  }
  
  // NOTE - This in an approximation...
  def isTaskKey[T](key: sbt.ScopedKey[T]): Boolean = {
    val mf = key.key.manifest
    mf.erasure == classOf[sbt.Task[_]]
  }
  def isInputKey[T](key: sbt.ScopedKey[T]): Boolean = {
    val mf = key.key.manifest
    mf.erasure == classOf[sbt.InputTask[_]]
  }
    
  
  private def shouldIncludeKey[T](filter: protocol.KeyFilter)(key: ScopedKey[T]): Boolean = {
    def configPasses: Boolean = 
      filter.config.map { c => 
        val opt = key.scope.config.toOption
        opt.isDefined && opt.get.name == c
      } getOrElse true
      
    def projectPasses: Boolean =
      filter.project.map { pr =>
        val opt = key.scope.project.toOption
        opt.collect {
          case x: ProjectRef => 
            // TODO - add build URI to filter...
            (x.project == pr.project) //&& (x.build == pr.build)
            true
        } getOrElse false
      } getOrElse true
    def keyPasses: Boolean =
      filter.key.map { keyname =>
        val opt = key.scope.task.toOption
        opt.isDefined && opt.get.label == keyname
      } getOrElse false
    // We should include if all fitlers pass (or are non-existent).
    configPasses && projectPasses && keyPasses
  }
}