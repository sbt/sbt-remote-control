package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext, Params, SimpleJsonMessage }
import com.typesafe.sbt.ui
import java.lang.reflect.{ Method, Proxy }
import com.typesafe.sbt.ui.SimpleJsonMessage
import scala.util.parsing.json.JSONObject
import sbt._

object PlaySupport {

  // Our mechanism of blocking until the user cancels.
  @annotation.tailrec
  final def blockForCancel(ctx: UIContext): Unit = {
    ctx.take() match {
      case ui.NoUIPresent => ()
      case ui.Canceled => ()
      case ui.Request(name, handle, sendError) =>
        // TODO - we should probably allow execution of things here...
        sendError("Request not supported during play run: " + name)
        blockForCancel(ctx)
    }
  }

  class PlayInteractionHandler(ui: UIContext) extends java.lang.reflect.InvocationHandler {
    def invoke(proxy: AnyRef, m: Method, args: Array[AnyRef]): AnyRef = {
      // Just hook after started...
      m.getName match {
        case "waitForCancel" =>
          PoorManDebug.debug("Play interaction hook: Waiting for cancelation.")
          blockForCancel(ui)
        case "doWithoutEcho" =>
          // Just Read function and run it.
          val func = args(0).asInstanceOf[Function0[Unit]]
          func()
        case "toString" =>
          return "PlayInteractionHandler(" + ui + ")"
        case name =>
        // We specifically ignore any other call.
      }
      ().asInstanceOf[AnyRef]
    }
  }
  // This is an ugly reflective hack to use our interaction rather than
  // play's.  We're in an alternative classloader though.
  def hackyAssignSetting[T](key: SettingKey[T], value: AnyRef): Setting[T] =
    key <<= Def.setting { value.asInstanceOf[T] }

  def makePlayInteractionSetting(key: ScopedKey[_], ui: UIContext): Setting[_] = {
    val interactionClass = key.key.manifest.runtimeClass
    PoorManDebug.trace("Installing interaction hook for class: " + interactionClass)
    val proxy =
      Proxy.newProxyInstance(
        interactionClass.getClassLoader,
        Array(interactionClass),
        new PlayInteractionHandler(ui))
    // This is an ugly reflective hack to use our interaction rather than
    // play's.  We're in an alternative classloader though.
    hackyAssignSetting(SettingKey(key.key) in key.scope, proxy)
  }
  /**
   * This class represents a dynamic proxy we use to avoid classpath hell when
   *  working with the play plugin.
   */
  class PlayRunHookHandler(ui: UIContext) extends java.lang.reflect.InvocationHandler {
    def invoke(proxy: AnyRef, m: Method, args: Array[AnyRef]): AnyRef = {
      // Just hook after started...
      m.getName match {
        case "afterStarted" =>
          PoorManDebug.debug("Play run hook: afterStarted called.")
          val socket = args(0).asInstanceOf[java.net.InetSocketAddress]
          val msg = SimpleJsonMessage(JSONObject(Map(
            "host" -> socket.getHostName,
            "port" -> socket.getPort)))
          ui.sendEvent("playServerStarted", msg)
          null
        case "toString" =>
          "PlayRunHookHandler(" + ui + ")"
        case name =>
          // We specifically ignore all other requests
          null
      }
    }
    // return something
  }

  def hackyAddToTask[T](key: TaskKey[Seq[T]], element: Any): Setting[Task[Seq[T]]] =
    key := {
      (element.asInstanceOf[T] +: key.value)
    }

  def makeDynamicProxyRunHookSetting(key: ScopedKey[_], ui: UIContext): Setting[_] = {
    val mf = key.key.manifest
    // Manfiest is a Task[Seq[PlayRunHook]], so we want the first type argument of
    // the first type argument....
    val runHookClass = mf.typeArguments(0).typeArguments(0).runtimeClass
    PoorManDebug.trace("Creating play run hook for class: " + runHookClass)
    val proxy =
      Proxy.newProxyInstance(
        runHookClass.getClassLoader,
        Array(runHookClass),
        new PlayRunHookHandler(ui))
    // This is a very sketchy reflective hack.
    hackyAddToTask(TaskKey(key.key.asInstanceOf[AttributeKey[Task[Seq[AnyRef]]]]) in key.scope, proxy)
  }

  // New proxy
  // java.lang.reflect.Proxy.newProxyInstance(obj.getClass().getClassLoader(),
  //                                        Class[] { MyProxyInterface.class },
  //                                        new MyDynamicProxyClass(obj));

  def findPlaySettingScopedKeys(name: String, settings: Seq[Setting[_]]): Seq[ScopedKey[_]] = {
    // First find all the keys:
    val keys =
      (for {
        setting <- settings
        if (setting.key.key.label == name) || (setting.key.key.rawLabel == name)
      } yield setting.key)
    // TODO - does distinct work?
    keys.distinct
  }

  // Adds our hooks into the play build.
  def installHooks(state: State, ui: UIContext): State = {
    PoorManDebug.debug("Installing play hooks.")
    val extracted = Project.extract(state)
    val settings = extracted.session.mergeSettings
    val runHookKeys = findPlaySettingScopedKeys("playRunHooks", settings)
    if (runHookKeys.isEmpty)
      sys.error("Unable to find play run hook!  Possibly incompatible play version.")
    // We can ignore the atmos specific ones:
    val nonAtmosKeys = runHookKeys.filterNot(_.scope.config.fold(config => config.name == "atmos", ifGlobal = false, ifThis = false))
    val fixedHooks = nonAtmosKeys map (setting => makeDynamicProxyRunHookSetting(setting, ui))
    val interactionKeys = findPlaySettingScopedKeys("playInteractionMode", settings)
    if (interactionKeys.isEmpty)
      sys.error("Unable to find play itneraction hook!  Possibly incompatible play version.")
    val fixedInteractions = interactionKeys map { setting => makePlayInteractionSetting(setting, ui) }
    val newSettings: Seq[Setting[_]] = fixedHooks ++ fixedInteractions
    SbtUtil.reloadWithAppended(state, newSettings)
  }

  def installPlaySupport(origState: State, ui: UIContext): State = {
    PoorManDebug.trace("Checking if play hooks are needed.")
    if (isPlayProject(origState)) installHooks(origState, ui)
    else origState
  }
}