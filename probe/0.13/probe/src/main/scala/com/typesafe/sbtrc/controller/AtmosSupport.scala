package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext, Params, SimpleJsonMessage }
import com.typesafe.sbt.ui
import java.lang.reflect.{ Method, Proxy }
import com.typesafe.sbt.ui.SimpleJsonMessage
import scala.util.parsing.json.JSONObject
import sbt._

object AtmosSupport {

  // Hacker to add a value to a task sequence (somewhat ignoring runtime types).
  def hackyAddToTask[T](key: TaskKey[Seq[T]], element: Any): Setting[Task[Seq[T]]] =
    key := {
      (element.asInstanceOf[T] +: key.value)
    }

  def makeAtmosRunHook(exampleSetting: Setting[_], ui: UIContext): Setting[_] = {
    val key = exampleSetting.key
    def eventMonitor(uri: URI): Unit = {
      // TODO - Formalize this as a case class?
      ui.sendEvent("atmosStarted", SimpleJsonMessage(JSONObject(Map("uri" -> uri.toASCIIString()))))
    }
    val listener: URI => Unit = eventMonitor _
    hackyAddToTask(TaskKey(key.key.asInstanceOf[AttributeKey[Task[Seq[AnyRef]]]]) in key.scope, listener)
  }

  def findAtmosSetting(name: String, settings: Seq[Setting[_]]): Option[Setting[_]] =
    (for {
      setting <- settings
      if (setting.key.key.label == name) || (setting.key.key.rawLabel == name)
    } yield setting).headOption

  // Adds our hooks into the Atmos build.
  def installHooks(state: State, ui: UIContext): State = {
    PoorManDebug.debug("Installing atmos hooks.")
    val (extracted, ref) = SbtUtil.extractWithRef(state)
    val settings = extracted.session.mergeSettings
    val runHookKey = findAtmosSetting("atmos-run-listeners", settings).getOrElse(
      sys.error("Unable to find play run hook!  Possibly incompatible play version."))
    val fixedHook = makeAtmosRunHook(runHookKey, ui)
    val newSettings = SbtUtil.makeAppendSettings(Seq[Setting[_]](fixedHook), ref, extracted)
    SbtUtil.reloadWithAppended(state, newSettings)
  }

  def isAtmosProject(state: State): Boolean = {
    PoorManDebug.trace("Checking if atmos hooks are needed.")
    val extracted = Project.extract(state)
    val settings = extracted.session.mergeSettings
    val supportsAtmos = findAtmosSetting("atmos-run-listeners", settings).isDefined
    val supportsAkka =
      if (AkkaSupport.isAkkaProject(state)) AkkaSupport.validAkkaVersion(state, BuildInfo.supportedAkkaVersionSbt013)
      else true
    val supportsPlay =
      if (isPlayProject(state)) PlaySupport.validPlayVersion(state, BuildInfo.supportedPlayVersionSbt013)
      else true
    supportsAtmos && supportsAkka && supportsPlay
  }

  def installAtmosSupport(origState: State, ui: UIContext): State = {
    if (isAtmosProject(origState)) {
      installHooks(origState, ui)
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

  def convertVersionString(version: String): Int = {
    val index = if (version.contains("-")) version.indexOf("-") else version.length
    version.substring(0, index).replace(".", "").toInt
  }
}