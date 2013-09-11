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

  def extractWithRef(state: State, context: Option[UIContext] = None): (Extracted, ProjectRef) = {
    val state2: State = context match {
      case Some(ui) => reloadWithUiContext(state, ui)
      case None => state
    }
    val extracted = Project.extract(state2)
    (Extracted(extracted.structure, extracted.session, extracted.currentRef)(showFullKey(state2)), extracted.currentRef)
  }

  def extract(state: State, context: Option[UIContext] = None): Extracted = {
    extractWithRef(state, context)._1
  }

  def runInputTask[T](key: sbt.ScopedKey[T], state: State, args: String, context: Option[UIContext] = None): State = {
    PoorManDebug.trace("Running input task: " + key)
    val extracted = extract(state, context)
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
  
  def runCommand(command: String, state: State, context: Option[UIContext] = None): State = {
    // TODO - We may need to adapt state so we can run a command against a particular project/ref.  Commands are
    // attached to projects, so this gets odd.
    def loadUi(ui: UIContext): State = reloadWithUiContext(state,ui)
    val realState = context map loadUi getOrElse state
    sbt.Command.process(command, realState)
  }

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
    PoorManDebug.trace("Appending " + appendSettings + " to state.")
    // reloads with appended settings.
    val session = Project.session(state)
    val structure = Project.structure(state)
    implicit val display = Project.showContextKey(state)
    // When we reload, make sure we keep all reapplied settings...
    //val newStructure = Load.reapply(session.mergeSettings ++ appendSettings, structure)
    val newSession = session.appendRaw(appendSettings)
    // updates various aspects of State based on the new settings
    // and returns the updated State
    SessionSettings.reapply(newSession, state)
  }
}
