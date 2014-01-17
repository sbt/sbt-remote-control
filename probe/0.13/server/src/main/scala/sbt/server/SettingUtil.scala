package sbt.server

import _root_.sbt._
import sbt.Keys._
import sbt.Defaults._

// TODO - We should probably rename this to "SettingUtil" to represent what it's actualyl doing for us.
private[server] object SettingUtil {

  /** A helper method to ensure that settings we're appending are scoped according to the current project ref. */
  def makeAppendSettings(settings: Seq[Setting[_]], inProject: ProjectRef, extracted: Extracted) = {
    // transforms This scopes in 'settings' to be the desired project
    val appendSettings = Load.transformSettings(Load.projectScope(inProject), inProject.build, extracted.rootProject, settings)
    appendSettings
  }

  /** Reloads an sbt build with the given settings being appended to the current session. */
  def reloadWithAppended(state: State, appendSettings: Seq[sbt.Setting[_]]): State = {
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
