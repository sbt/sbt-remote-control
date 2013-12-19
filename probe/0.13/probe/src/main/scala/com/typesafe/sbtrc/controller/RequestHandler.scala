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

// TODO - See how much of this we can share between sbt versions....
object RequestHandler {

  import SbtUtil._

  /** This class knows how to handle requests for sbt 0.13.  */
  def handleRequest(s: State, context: UIContext, request: protocol.Request): RequestResult =
    try request match {
      // High level API
      case req: protocol.NameRequest => nameHandler(s, context, req)
      case req: protocol.MainClassRequest => mainClassHandler(s, context, req)
      case req: protocol.WatchTransitiveSourcesRequest => watchTransitiveSourcesHandler(s, context, req)
      case req: protocol.CompileRequest => compileHandler(s, context, req)
      // TODO - Just delegate to one handler that can extract itself appropriately
      case req @ protocol.RunRequest(_, _, None, false) => runHandler(run in Compile, "run")(s, context, req)
      case req @ protocol.RunRequest(_, _, None, true) => runHandler(run in (config("atmos")), "run:atmos")(s, context, req)
      case req @ protocol.RunRequest(_, _, Some(_), false) => runMainHandler(runMain in Compile, "run-main")(s, context, req)
      case req @ protocol.RunRequest(_, _, Some(_), true) => runMainHandler(runMain in config("atmos"), "atmos:run-main")(s, context, req)
      case req: protocol.TestRequest => testHandler(s, context, req)
      // Low-Level API
      case req: protocol.SettingKeyRequest => settingKeyHandler(s, context, req)
      case req: protocol.SettingValueRequest => settingValueHandler(s, context, req)
      case req: protocol.TaskKeyRequest => taskKeyHandler(s, context, req)
      case req: protocol.TaskValueRequest => taskValueHandler(s, context, req)
      case req: protocol.InputTaskKeyRequest => inputTaskKeyHandler(s, context, req)
      case req: protocol.ExecuteCommandRequest => commandHandler(s, context, req)
      case _ => RequestNotHandled
    } catch {
      case e: Exception => RequestFailure(e)
    }

  private def nameHandler(origState: State, ui: UIContext, params: protocol.NameRequest): RequestResult = {
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
    RequestSuccess(protocol.NameResponse(results), origState)
  }

  private def mainClassHandler(origState: State, ui: UIContext, req: protocol.MainClassRequest): RequestResult = {
    PoorManDebug.debug("Running `mainClass` task.")
    val ref = req.ref
    val extracted = extract(origState, Some(ui))
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
    RequestSuccess(protocol.MainClassResponse(results), origState)
  }

  private def watchTransitiveSourcesHandler(origState: State, ui: UIContext, params: protocol.WatchTransitiveSourcesRequest): RequestResult = {
    val (s, result) = extract(origState).runTask(watchTransitiveSources, origState)
    RequestSuccess(protocol.WatchTransitiveSourcesResponse(files = result), s)
  }

  private def compileHandler(origState: State, ui: UIContext, req: protocol.CompileRequest): RequestResult = {
    val pref = req.ref
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
    RequestSuccess(protocol.CompileResponse(results), state)
  }

  private def runHandler[T](key: sbt.InputKey[T], taskName: String)(origState: State, ui: UIContext, req: protocol.RunRequest): RequestResult = {
    PoorManDebug.debug("Invoking the run task in " + key.scope.config)
    val key2 = req.ref match {
      case Some(r) => key in SbtToProtocolUtils.projectRefFromProtocol(r)
      case _ => key
    }
    val s = runInputTask(key2, origState, args = "", Some(ui))
    RequestSuccess(protocol.RunResponse(success = true, task = taskName), origState)
  }

  private def runMainHandler[T](key: sbt.InputKey[T], taskName: String)(origState: State, ui: UIContext, req: protocol.RunRequest): RequestResult = {
    val ref = req.ref
    // This should be safe because of pattern match on handler...
    val Some(klass) = req.mainClass
    PoorManDebug.debug("Invoking the run-main task in " + key.scope.config)
    val key2: sbt.InputKey[T] = ref match {
      case Some(r) => key in SbtToProtocolUtils.projectRefFromProtocol(r)
      case _ => key
    }
    // Note: For now this is safe. In the future, let's just not cast 30 bajillion times.
    val s = runInputTask(key2, origState, args = klass, Some(ui))
    RequestSuccess(protocol.RunResponse(success = true, task = taskName), origState)
  }

  private def testHandler(origState: State, ui: UIContext, req: protocol.TestRequest): RequestResult = {
    PoorManDebug.debug("Invoking the test task.")
    val extracted = extract(origState)

    // DEBUG ONLY

    val key = req.ref match {
      case Some(ref) => executeTests in Test in SbtToProtocolUtils.projectRefFromProtocol(ref)
      // TODO - in this case, we may want to aggregate...
      case _ => executeTests in Test
    }
    val (s2, result) = extract(origState).runTask(key, origState)
    val outcome = result.overall match {
      case TestResult.Error => protocol.TestError
      case TestResult.Passed => protocol.TestPassed
      case TestResult.Failed => protocol.TestFailed
    }
    RequestSuccess(protocol.TestResponse(outcome), origState)
  }

  private def commandHandler(origState: State, ui: UIContext, req: protocol.ExecuteCommandRequest): RequestResult = {
    PoorManDebug.debug("Invoking the comamnd [" + req.command + "]")
    RequestSuccess(protocol.ExecuteCommandResponse(), runCommand(req.command, origState, Some(ui)))
  }

  private def extractValue[T](key: sbt.ScopedKey[T], state: State): protocol.TaskResult[T] =
    try {
      val raw = extract(state).get(SettingKey(key.key) in key.scope)
      val value = protocol.BuildValue(raw)(key.key.manifest)
      protocol.TaskSuccess(value)
    } catch {
      case e: Exception => protocol.TaskFailure(e.getMessage())
    }

  private def settingValueHandler(origState: State, ui: UIContext, req: protocol.SettingValueRequest): RequestResult = {
    import protocol.{ ScopedKey => PScopedKey, TaskResult }
    // TODO - Catch errors
    PoorManDebug.debug("Looking up setting: " + req.key)
    val sbtKey: sbt.ScopedKey[_] = Sbt13ToProtocolUtils.protocolToScopedKey(req.key, origState)
    val value = extractValue(sbtKey, origState)
    RequestSuccess(protocol.SettingValueResponse(value), origState)
  }

  private def settingKeyHandler(origState: State, ui: UIContext, req: protocol.SettingKeyRequest): RequestResult = {
    import protocol.{ KeyFilter, KeyListResponse, KeyList }
    PoorManDebug.debug("Requesting setting keys: " + req.filter)
    val results =
      KeyList(SbtDiscovery.settings(origState, req.filter))
    RequestSuccess(KeyListResponse(results), origState)
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

  private def taskValueHandler(origState: State, ui: UIContext, req: protocol.TaskValueRequest): RequestResult = {
    import protocol.{ ScopedKey => PScopedKey, TaskResult }
    PoorManDebug.debug("Running task: " + req.key)
    val sbtKey: sbt.ScopedKey[_] = Sbt13ToProtocolUtils.protocolToScopedKey(req.key, origState)
    // TODO - Here we want to validate we have a task key using the manifest and issuing an error otherwise.
    val taskSbtKey = sbtKey.asInstanceOf[sbt.ScopedKey[Task[_]]]
    import language.existentials
    val (state, value) = runTaskByKey(taskSbtKey, origState)
    RequestSuccess(protocol.TaskValueResponse(value), origState)
  }

  private def taskKeyHandler(origState: State, ui: UIContext, req: protocol.TaskKeyRequest): RequestResult = {
    import protocol.{ KeyFilter, KeyListResponse, KeyList }
    PoorManDebug.debug("Requesting task keys: " + req.filter)
    val results =
      KeyList(SbtDiscovery.tasks(origState, req.filter))
    RequestSuccess(KeyListResponse(results), origState)
  }

  private def inputTaskKeyHandler(origState: State, ui: UIContext, req: protocol.InputTaskKeyRequest): RequestResult = {
    import protocol.{ KeyFilter, KeyListResponse, KeyList }
    val results =
      KeyList(SbtDiscovery.inputTasks(origState, req.filter))
    RequestSuccess(KeyListResponse(results), origState)
  }
}
