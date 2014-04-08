package com.typesafe.sbtrc

import _root_.sbt._
import Project.Initialize
import Keys._
import Defaults._
import Scope.GlobalScope
import sbt.Aggregation.KeyValue
import sbt.complete.DefaultParsers
import sbt.Load.BuildStructure
import com.typesafe.sbt.ui.{ Context => UIContext }
import com.typesafe.sbt.ui.SbtUiPlugin.uiContext
import SbtCustomHacks._

object SbtUtil {

  def extractWithRef(state: State): (Extracted, ProjectRef) = {
    val extracted = Project.extract(state)
    (Extracted(extracted.structure, extracted.session, extracted.currentRef)(showFullKey(state)), extracted.currentRef)
  }
  
  private def debugSettings(seq: Seq[Setting[_]]): Unit = {
    for {
      (setting, idx) <- seq.zipWithIndex
      key = setting.key.key
      config = setting.key.scope.config.toOption.map(_.name).getOrElse("*")
      project = setting.key.scope.project.toOption.map {
        case ProjectRef(uri, project) => project
        case x => x.toString
      }.getOrElse("*")
    } PoorManDebug.trace("  %03d. ".format(idx) + project + "/" + config + ":" + key)
  }

  def runInputTask[T](key: sbt.ScopedKey[T], state: State, args: String): State = {
    PoorManDebug.trace("Running input task: " + key)
    val extracted = Project.extract(state)
    PoorManDebug.trace("Additional raw settings:")
    debugSettings(extracted.session.rawAppend)
    implicit val display = Project.showContextKey(state)
    val it = extracted.get(SettingKey(key.key) in key.scope)
    val keyValues = KeyValue(key, it) :: Nil

    val parser = Aggregation.evaluatingParser(state, extracted.structure, show = dontShowAggregate)(keyValues)
    // we put a space in front of the args because the parsers expect
    // *everything* after the task name it seems
    DefaultParsers.parse(" " + args, parser) match {
      case Left(message) =>
        throw new Exception("Failed to run task: " + display(key) + ": " + message)
      case Right(f) =>
        f()
    }
  }
  
  def runCommand(command: String, state: State): State =
    sbt.Command.process(command, state)

  /** A helper method to ensure that settings we're appending are scoped according to the current project ref. */
  def makeAppendSettings(settings: Seq[Setting[_]], inProject: ProjectRef, extracted: Extracted) = {
    PoorManDebug.trace("Transforming " + settings + " to " + inProject)
    // transforms This scopes in 'settings' to be the desired project
    val appendSettings = Load.transformSettings(Load.projectScope(inProject), inProject.build, extracted.rootProject, settings)
    appendSettings
  }

  /** Reloads the project with the ui context attached to the current session as the latest change. */
  def reloadWithUiContext(state: State, context: UIContext): State = {
    PoorManDebug.trace("Adding " + context + " to state.")
    reloadWithAppended(state, Seq(uiContext in Global := context))
  }

  /** Reloads an sbt build with the given settings being appended to the current session. */
  def reloadWithAppended(state: State, appendSettings: Seq[sbt.Setting[_]]): State = {
    PoorManDebug.trace("Appending settings to state:")
    debugSettings(appendSettings)
    // reloads with appended settings.
    val session = Project.session(state)
    //val structure = Project.structure(state)
    //implicit val display = Project.showContextKey(state)
    // When we reload, make sure we keep all reapplied settings...
    //val newStructure = Load.reapply(session.mergeSettings ++ appendSettings, structure)
    val newSession = session.appendRaw(appendSettings)
    // updates various aspects of State based on the new settings
    // and returns the updated State
    SessionSettings.reapply(newSession, state)
  }
}
