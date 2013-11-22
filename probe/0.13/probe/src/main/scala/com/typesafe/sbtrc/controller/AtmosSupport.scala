package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext }
import com.typesafe.sbt.ui.SbtUiPlugin.uiContext
import com.typesafe.sbt.ui
import java.lang.reflect.{ Method, Proxy }
import scala.util.parsing.json.JSONObject
import sbt._

object AtmosSupport {

  // Helpers to create a global listenr from the global ui context setting.
  val uiAtmosListener = sbt.taskKey[URI => Unit]("An atmos listener which will fire events down the server channel.")
  val uiAtmosListenerGlobalSetting: Setting[_] =
    uiAtmosListener in Global := { uri =>
      PoorManDebug.trace("Sending atmos uri: " + uri.toASCIIString)
      (uiContext in Global).value.sendEvent("atmosStarted", Map("uri" -> uri.toASCIIString()))
    }

  def makeAtmosRunHooksImpl(keys: Seq[sbt.ScopedKey[_]]): Seq[Setting[_]] = {
    keys.map { key =>
      // Very hacky cast here...
      val taskKey = TaskKey(key.key.asInstanceOf[AttributeKey[Task[Seq[URI => Unit]]]])
      taskKey in key.scope +=
        (uiAtmosListener in Global).value
    } ++ Seq(uiAtmosListenerGlobalSetting)
  }

  def findAtmosKeys(name: String, settings: Seq[Setting[_]], ref: Option[ProjectRef] = None): Seq[ScopedKey[_]] =
    (for {
      setting <- settings
      if (setting.key.key.label == name) || (setting.key.key.rawLabel == name)
      // TODO - make sure this works!
      if (!ref.isDefined) || (setting.key.scope.project.toOption == ref)
    } yield setting.key).distinct

  // Adds our hooks into the Atmos build.
  def installHooks(state: State): State = {
    PoorManDebug.debug("Installing atmos hooks.")
    val (extracted, ref) = SbtUtil.extractWithRef(state)
    val settings = extracted.session.mergeSettings
    val runHookKeys = findAtmosKeys("atmos-run-listeners", settings)
    if (runHookKeys.isEmpty)
      sys.error("Unable to find atmos run hook!  Possibly incompatible atmos version.")
    val fixedHooks = makeAtmosRunHooksImpl(runHookKeys)
    val newSettings = SbtUtil.makeAppendSettings(fixedHooks, ref, extracted)
    SbtUtil.reloadWithAppended(state, newSettings)
  }

  def isAtmosProject(state: State, ref: Option[ProjectRef] = None): Boolean = {
    PoorManDebug.trace("Checking if atmos hooks are needed.")
    val extracted = Project.extract(state)
    val settings = extracted.session.mergeSettings
    !findAtmosKeys("atmos-run-listeners", settings, ref).isEmpty
  }

  def installAtmosSupport(origState: State): State = {
    if (isAtmosProject(origState)) {
      installHooks(origState)
    } else origState
  }

  import io.{ ShimWriter, GenericShimWriter }
  import ShimWriter.{
    atmosPlayPluginShim,
    atmosPlayBuildShim,
    atmosPluginShim,
    atmosAkkaBuildShim,
    atmosPluginDeleteShim,
    atmosAkkaBuildDeleteShim,
    atmosPlayPluginDeleteShim,
    atmosPlayBuildDeleteShim
  }

  def getAtmosShims(state: State): Seq[ShimWriter] = {
    // TODO - Detect play/akka by project.
    val isPlay = isPlayProject(state)
    val isAkka = AkkaSupport.isAkkaProject(state)
    // TODO - When we have the latest atmos plugin we can include the build shim
    // TODO - We need a shim to turn off the atmosBuildShim if an akka project migrates to play...
    if (isPlay) {
      PoorManDebug.trace("Play+Atmos hooks are needed.")
      Seq(atmosPlayPluginShim, atmosPlayBuildShim,
        // When installing Play support, make sure we delete Akka support,
        // or things get wonky.
        atmosAkkaBuildDeleteShim, atmosPluginDeleteShim)
    } else if (isAkka) {
      PoorManDebug.trace("Akka+Atmos hooks are needed.")
      // We have to also delete Play atmos support if migrating from
      // play -> just akka.
      Seq(atmosPluginShim, atmosAkkaBuildShim,
        atmosPlayPluginDeleteShim, atmosPlayBuildDeleteShim)
    } else Nil
  }
}