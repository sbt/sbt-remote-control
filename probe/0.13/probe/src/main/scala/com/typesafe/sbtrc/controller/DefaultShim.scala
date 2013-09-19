package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext, Params }
import _root_.sbt._
import sbt.Keys._
import sbt.Defaults._
import org.scalatools.testing._
import sbt.testing.{ Status => TStatus }
import SbtUtil.extract
import SbtUtil.extractWithRef
import SbtUtil.reloadWithAppended
import SbtUtil.runInputTask
import protocol.TaskNames
import ParamsHelper.p2Helper
import protocol.{ JsonStructure, RawStructure }
import com.typesafe.sbt.ui.{ Context => UIContext }
import sbt.testing.{ Status => TStatus }

object DefaultsShim {

  import SbtUtil._
  import protocol.TaskNames

  private def sendEvent(ui: UIContext, id: String, paramsMap: Map[String, Any]): Unit = {
    ui.sendEvent(id, ParamsHelper.fromMap(paramsMap))
  }

  private[sbtrc] def makeResponseParams(specific: protocol.SpecificResponse): Params = {
    ParamsHelper.fromMap(specific.toGeneric.params)
  }

  private[sbtrc] def Response[T: RawStructure](value: T) =
    ParamsHelper.toParams(value)

  private val listenersKey = testListeners in Test

  private def addTestListener(state: State, ui: UIContext): State = {
    PoorManDebug.trace("Adding test lisener to state.")
    val (extracted, ref) = extractWithRef(state)
    val ourListener = new UiTestListener(ui, extracted.get(listenersKey))

    val settings = makeAppendSettings(Seq(listenersKey <<= (listenersKey) map { listeners =>
      listeners :+ ourListener
    }), ref, extracted)

    reloadWithAppended(state, settings)
  }

  private def removeTestListener(state: State, ui: UIContext): (State, protocol.TestOutcome) = {
    val ref = Project.extract(state).currentRef
    val extracted = Extracted(Project.structure(state), Project.session(state), ref)(SbtCustomHacks.showFullKey(state))

    val (s1, listeners) = extracted.runTask(listenersKey, state)

    val ours = listeners.collect({
      case l: UiTestListener if l.ui eq ui => l
    }).headOption
      .getOrElse(throw new RuntimeException("Our test listener wasn't installed!"))

    // put back the original listener task
    val settings = makeAppendSettings(Seq(
      Def.setting(listenersKey, Def.value(ours.oldTask))), ref, extracted)

    (reloadWithAppended(s1, settings), ours.overallOutcome)
  }

  private val nameHandler: RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Extracting name and capabilities of this build.")
    val result = extract(origState).get(name)

    // TODO - These are all hacks for now until we have the generic API.
    val hasPlay = controller.isPlayProject(origState)
    val hasConsole = AtmosSupport.isAtmosProject(origState)
    val hasAkka = AkkaSupport.isAkkaProject(origState)

    (origState, makeResponseParams(protocol.NameResponse(result,
      Map("hasPlay" -> hasPlay,
        "hasAkka" -> hasAkka,
        "hasConsole" -> hasConsole))))
  }

  private val mainClassHandler: RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Running `mainClass` task.")
    val (s, result) = extract(origState).runTask(mainClass in Compile in run, origState)
    (s, makeResponseParams(protocol.MainClassResponse(name = result)))
  }

  private val discoveredMainClassesHandler: RequestHandler = { (origState, ui, params) =>
    val (s, result) = extract(origState).runTask(discoveredMainClasses in Compile in run, origState)
    (s, makeResponseParams(protocol.DiscoveredMainClassesResponse(names = result)))
  }

  private val watchTransitiveSourcesHandler: RequestHandler = { (origState, ui, params) =>
    val (s, result) = extract(origState).runTask(watchTransitiveSources, origState)
    (s, makeResponseParams(protocol.WatchTransitiveSourcesResponse(files = result)))
  }

  private val compileHandler: RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Compiling the project.")
    val (s, result) = extract(origState).runTask(compile in Compile, origState)
    (s, makeResponseParams(protocol.CompileResponse(success = true)))
  }

  private def makeRunHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Invoking the run task in " + key.scope.config)
    val shimedState = installShims(origState, ui)
    val s = runInputTask(key, shimedState, args = "", Some(ui))
    (origState, makeResponseParams(protocol.RunResponse(success = true,
      task = taskName)))
  }

  private val runHandler: RequestHandler = makeRunHandler(run in Compile, protocol.TaskNames.run)

  private val runAtmosHandler: RequestHandler = makeRunHandler(run in (config("atmos")), protocol.TaskNames.runAtmos)

  private def makeRunMainHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Invoking the run-main task in " + key.scope.config)
    import ParamsHelper._
    val shimedState = installShims(origState, ui)
    val klass = params.toMap.get("mainClass")
      .map(_.asInstanceOf[String])
      .getOrElse(throw new RuntimeException("need to specify mainClass in params"))
    val s = runInputTask(key, shimedState, args = klass, Some(ui))
    (origState, makeResponseParams(protocol.RunResponse(success = true,
      task = taskName)))
  }

  private val runMainHandler: RequestHandler = makeRunMainHandler(runMain in Compile, protocol.TaskNames.runMain)

  private val runMainAtmosHandler: RequestHandler = makeRunMainHandler(runMain in config("atmos"), protocol.TaskNames.runMainAtmos)

  private val testHandler: RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Invoking the test task.")
    val shimedState = installShims(origState, ui)
    val (s2, result1) = extract(shimedState).runTask(test in Test, shimedState)
    val (s3, outcome) = removeTestListener(s2, ui)
    (origState, makeResponseParams(protocol.TestResponse(outcome)))
  }

  private def commandHandler(command: String): RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Invoking the comamnd [" + command + "]")
    val shimedState = installShims(origState, ui)
    runCommand(command, shimedState, Some(ui)) -> Params("application/json", "{}")
  }

  /** This installs all of our shim hooks into the project. */
  def installShims(origState: State, ui: UIContext): State = {
    val s1 = addTestListener(origState, ui)
    val s2 = PlaySupport.installPlaySupport(s1, ui)
    val s3 = AtmosSupport.installAtmosSupport(s2, ui)
    s3
  }

  private def extractValue[T](key: sbt.ScopedKey[T], state: State): protocol.TaskResult[T] =
    try {
      val raw = extract(state).get(SettingKey(key.key) in key.scope)
      val value = protocol.BuildValue(raw)(key.key.manifest)
      protocol.TaskSuccess(value)
    } catch {
      case e: Exception => protocol.TaskFailure(e.getMessage())
    }

  private val settingValueHandler: RequestHandler = { (origState, ui, params) =>
    import protocol.{ ScopedKey => PScopedKey, TaskResult }
    // TODO - Catch errors
    val key = JsonStructure.unapply[PScopedKey](params.toMap).get
    PoorManDebug.debug("Looking up setting: " + key)
    val sbtKey: sbt.ScopedKey[_] = Sbt13ToProtocolUtils.protocolToScopedKey(key, origState)
    val value = extractValue(sbtKey, origState)
    (origState, Response(value))
  }

  private val settingKeyHandler: RequestHandler = { (origState, ui, params) =>
    import protocol.{ KeyFilter, KeyListResponse, KeyList }
    val filter = JsonStructure.unapply[KeyFilter](params.toMap).get
    PoorManDebug.debug("Requesting setting keys: " + filter)
    val results =
      KeyList(SbtDiscovery.settings(origState, filter))
    (origState, Response(results))
  }

  private def runTaskByKey0[T](key: sbt.ScopedKey[Task[T]], state: State): (State, protocol.TaskResult[T]) =
    try {
      val (state2, raw) = extract(state).runTask(sbt.TaskKey(key.key) in key.scope, state)
      val mf: Manifest[Task[T]] = key.key.manifest
      val rawManifest: Manifest[T] = mf.typeArguments(0).asInstanceOf[Manifest[T]]
      val value = protocol.BuildValue(raw)(rawManifest)
      state2 -> protocol.TaskSuccess(value)
    } catch {
      case e: Exception => state -> protocol.TaskFailure(e.getMessage())
    }

  // Hackery to get around type system fun.!
  private def runTaskByKey(key: sbt.ScopedKey[_], state: State): (State, protocol.TaskResult[_]) =
    runTaskByKey0(key.asInstanceOf[ScopedKey[Task[Any]]], state)

  private val taskValueHandler: RequestHandler = { (origState, ui, params) =>
    import protocol.{ ScopedKey => PScopedKey, TaskResult }
    val key = JsonStructure.unapply[PScopedKey](params.toMap).get
    PoorManDebug.debug("Running task: " + key)
    val sbtKey: sbt.ScopedKey[_] = Sbt13ToProtocolUtils.protocolToScopedKey(key, origState)
    // TODO - Here we want to validate we have a task key using the manifest and issuing an error otherwise.
    val taskSbtKey = sbtKey.asInstanceOf[sbt.ScopedKey[Task[_]]]
    val (state, value) = runTaskByKey(taskSbtKey, installShims(origState, ui))
    (origState, Response(value))
  }

  private val taskKeyHandler: RequestHandler = { (origState, ui, params) =>
    import protocol.{ KeyFilter, KeyListResponse, KeyList }
    val filter = JsonStructure.unapply[KeyFilter](params.toMap).get
    PoorManDebug.debug("Requesting task keys: " + filter)
    val results =
      KeyList(SbtDiscovery.tasks(origState, filter))
    (origState, Response(results))
  }

  private val inputTaskKeyHandler: RequestHandler = { (origState, ui, params) =>
    import protocol.{ KeyFilter, KeyListResponse, KeyList }
    val filter = JsonStructure.unapply[KeyFilter](params.toMap).get
    val results =
      KeyList(SbtDiscovery.inputTasks(origState, filter))
    (origState, Response(results))
  }

  // TODO - this whole mechanism needs work.  We should just have generic:
  // * Return value of setting
  // * Run a task
  // * Run an input task
  // * Run a command
  val findHandler: PartialFunction[String, RequestHandler] = {
    case TaskNames.name => nameHandler
    case TaskNames.mainClass => mainClassHandler
    case TaskNames.discoveredMainClasses => discoveredMainClassesHandler
    case TaskNames.watchTransitiveSources => watchTransitiveSourcesHandler
    case TaskNames.compile => compileHandler
    case TaskNames.run => runHandler
    case TaskNames.runMain => runMainHandler
    case TaskNames.runAtmos => runAtmosHandler
    case TaskNames.runMainAtmos => runMainAtmosHandler
    case TaskNames.test => testHandler
    // Generic API
    case TaskNames.SettingKeyRequest => settingKeyHandler
    case TaskNames.TaskKeyRequest => taskKeyHandler
    case TaskNames.InputTaskKeyRequest => inputTaskKeyHandler
    case TaskNames.SettingValueRequest => settingValueHandler
    case TaskNames.TaskValueRequest => taskValueHandler

    // Old API
    case name @ ("eclipse" | "gen-idea") => commandHandler(name)
  }
}
