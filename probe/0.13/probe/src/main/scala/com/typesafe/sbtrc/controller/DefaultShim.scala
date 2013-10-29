package com.typesafe.sbtrc
package controller

import com.typesafe.sbt.ui.{ Context => UIContext }
import _root_.sbt._
import sbt.Keys._
import sbt.Defaults._
import org.scalatools.testing._
import sbt.testing.{ Status => TStatus }
import SbtUtil.extract
import SbtUtil.extractWithRef
import SbtUtil.reloadWithAppended
import SbtUtil.runInputTask
import protocol.{ JsonStructure, RawStructure }
import com.typesafe.sbt.ui.{ Context => UIContext }
import sbt.testing.{ Status => TStatus }

object DefaultsShim {

  import SbtUtil._

  private def sendEvent(ui: UIContext, id: String, paramsMap: Map[String, Any]): Unit = {
    ui.sendEvent(id, paramsMap)
  }

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

    // TODO - Do this for every major project
    val extracted = extract(origState)
    val results =
      for {
        ref <- extracted.structure.allProjectRefs
      } yield {
        val name = extracted.get(sbt.Keys.name in ref)
        val hasPlay = controller.isPlayProject(origState, Some(ref))
        val hasConsole = AtmosSupport.isAtmosProject(origState, Some(ref))
        val hasAkka = AkkaSupport.isAkkaProject(origState, Some(ref))
        protocol.ProjectInfo(
          SbtToProtocolUtils.projectRefToProtocol(ref),
          name,
          true,
          Map("hasPlay" -> hasPlay,
            "hasAkka" -> hasAkka,
            "hasConsole" -> hasConsole))
      }
    (origState, protocol.NameResponse(results))
  }

  private val mainClassHandler: RequestHandler = {
    case (origState, ui, protocol.MainClassRequest(_, ref)) =>
      PoorManDebug.debug("Running `mainClass` task.")
      val extracted = extract(origState)

      val refs = ref.map(r => Seq(SbtToProtocolUtils.projectRefFromProtocol(r))).getOrElse(extracted.structure.allProjectRefs)
      val results =
        for (ref <- refs) yield {
          val (_, mc) = extracted.runTask(mainClass in Compile in run in ref, origState)
          val (_, discoveredMc) = extracted.runTask(discoveredMainClasses in Compile in run in ref, origState)
          protocol.DiscoveredMainClasses(
            SbtToProtocolUtils.projectRefToProtocol(ref),
            mainClasses = discoveredMc,
            defaultMainClass = mc)
        }
      (origState, protocol.MainClassResponse(results))
  }

  private val watchTransitiveSourcesHandler: RequestHandler = { (origState, ui, params) =>
    val (s, result) = extract(origState).runTask(watchTransitiveSources, origState)
    (s, protocol.WatchTransitiveSourcesResponse(files = result))
  }

  private val compileHandler: RequestHandler = {
    case (origState, ui, protocol.CompileRequest(_, pref)) =>
      PoorManDebug.debug("Compiling the project: " + pref)
      // TODO - handle aggregation....
      val extracted = extract(origState)
      val refs = pref match {
        case Some(r) => Seq(SbtToProtocolUtils.projectRefFromProtocol(r))
        case _ => extracted.structure.allProjectRefs
      }
      // Fold over all the refs and run the compile task.
      val (state, results) =
        ((origState -> Vector.empty[protocol.CompileResult]) /: refs) {
          case ((state, results), ref) =>
            val key = compile in Compile in ref
            val pref = SbtToProtocolUtils.projectRefToProtocol(ref)
            try {
              val (s, result) = extracted.runTask(key, state)
              state -> (results :+ protocol.CompileResult(pref, true))
            } catch {
              case e: Exception =>
                // TODO - just catch task exceptions
                state -> (results :+ protocol.CompileResult(pref, false))
            }
        }
      (state, protocol.CompileResponse(results))
  }

  private def makeRunHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Invoking the run task in " + key.scope.config)
    val shimedState = installShims(origState, ui)
    val s = runInputTask(key, shimedState, args = "", Some(ui))
    (origState, protocol.RunResponse(success = true,
      task = taskName))
  }

  private val runHandler: RequestHandler = makeRunHandler(run in Compile, "run")

  private val runAtmosHandler: RequestHandler = makeRunHandler(run in (config("atmos")), "run:atmos")

  private def makeRunMainHandler[T](key: sbt.ScopedKey[T], taskName: String): RequestHandler = { (origState, ui, request) =>
    PoorManDebug.debug("Invoking the run-main task in " + key.scope.config)
    // Note: For now this is safe. In the future, let's just not cast 30 bajillion times.
    val runRequest = request.asInstanceOf[protocol.RunRequest]
    val shimedState = installShims(origState, ui)
    val klass = runRequest.mainClass.getOrElse(throw new RuntimeException("need to specify mainClass in params"))
    val s = runInputTask(key, shimedState, args = klass, Some(ui))
    (origState, protocol.RunResponse(success = true, task = taskName))
  }

  private val runMainHandler: RequestHandler = makeRunMainHandler(runMain in Compile, "run-main")

  private val runMainAtmosHandler: RequestHandler = makeRunMainHandler(runMain in config("atmos"), "atmos:run-main")

  private val testHandler: RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Invoking the test task.")
    val shimedState = installShims(origState, ui)
    val (s2, result1) = extract(shimedState).runTask(test in Test, shimedState)
    val (s3, outcome) = removeTestListener(s2, ui)
    (origState, protocol.TestResponse(outcome))
  }

  private def commandHandler(command: String): RequestHandler = { (origState, ui, params) =>
    PoorManDebug.debug("Invoking the comamnd [" + command + "]")
    val shimedState = installShims(origState, ui)
    runCommand(command, shimedState, Some(ui)) -> protocol.ExecuteCommandResponse()
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

  private val settingValueHandler: RequestHandler = {
    case (origState, ui, protocol.SettingValueRequest(key)) =>
      import protocol.{ ScopedKey => PScopedKey, TaskResult }
      // TODO - Catch errors
      PoorManDebug.debug("Looking up setting: " + key)
      val sbtKey: sbt.ScopedKey[_] = Sbt13ToProtocolUtils.protocolToScopedKey(key, origState)
      val value = extractValue(sbtKey, origState)
      (origState, protocol.SettingValueResponse(value))
  }

  private val settingKeyHandler: RequestHandler = {
    case (origState, ui, protocol.SettingKeyRequest(filter)) =>
      import protocol.{ KeyFilter, KeyListResponse, KeyList }
      PoorManDebug.debug("Requesting setting keys: " + filter)
      val results =
        KeyList(SbtDiscovery.settings(origState, filter))
      (origState, KeyListResponse(results))
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

  private val taskValueHandler: RequestHandler = {
    case (origState, ui, protocol.TaskValueRequest(key, _)) =>
      import protocol.{ ScopedKey => PScopedKey, TaskResult }
      PoorManDebug.debug("Running task: " + key)
      val sbtKey: sbt.ScopedKey[_] = Sbt13ToProtocolUtils.protocolToScopedKey(key, origState)
      // TODO - Here we want to validate we have a task key using the manifest and issuing an error otherwise.
      val taskSbtKey = sbtKey.asInstanceOf[sbt.ScopedKey[Task[_]]]
      val (state, value) = runTaskByKey(taskSbtKey, installShims(origState, ui))
      (origState, protocol.TaskValueResponse(value))
  }

  private val taskKeyHandler: RequestHandler = {
    case (origState, ui, protocol.TaskKeyRequest(filter)) =>
      import protocol.{ KeyFilter, KeyListResponse, KeyList }
      PoorManDebug.debug("Requesting task keys: " + filter)
      val results =
        KeyList(SbtDiscovery.tasks(origState, filter))
      (origState, KeyListResponse(results))
  }

  private val inputTaskKeyHandler: RequestHandler = {
    case (origState, ui, protocol.InputTaskKeyRequest(filter)) =>
      import protocol.{ KeyFilter, KeyListResponse, KeyList }
      val results =
        KeyList(SbtDiscovery.inputTasks(origState, filter))
      (origState, KeyListResponse(results))
  }

  // TODO - this whole mechanism needs work.
  // We don't we just EXECUTE the dang task rather than returning a task that
  // can execute a task?
  val findHandler: PartialFunction[protocol.Request, RequestHandler] = {
    case _: protocol.NameRequest => nameHandler
    case _: protocol.MainClassRequest => mainClassHandler
    case _: protocol.WatchTransitiveSourcesRequest => watchTransitiveSourcesHandler
    case _: protocol.CompileRequest => compileHandler
    case protocol.RunRequest(_, _, None, false) => runHandler
    case protocol.RunRequest(_, _, Some(_), false) => runMainHandler
    // TODO - Fix Atmos run commands!
    case protocol.RunRequest(_, _, None, true) => runAtmosHandler
    case protocol.RunRequest(_, _, Some(_), true) => runMainAtmosHandler
    case _: protocol.TestRequest => testHandler
    // Generic API
    case _: protocol.SettingKeyRequest => settingKeyHandler
    case _: protocol.TaskKeyRequest => taskKeyHandler
    case _: protocol.InputTaskKeyRequest => inputTaskKeyHandler
    case _: protocol.SettingValueRequest => settingValueHandler
    case _: protocol.TaskValueRequest => taskValueHandler
    case protocol.ExecuteCommandRequest(command, _) => commandHandler(command)
  }
}
