package sbt

import complete.Parser
import complete.DefaultParsers
import Keys._
import Def.Initialize
import sbinary.DefaultProtocol.StringFormat
import Cache.seqFormat
import Attributed.data

object SbtBackgroundRunPlugin extends AutoPlugin {
  override def trigger = AllRequirements
  override def requires = plugins.JvmPlugin

  override val globalSettings: Seq[Setting[_]] = Seq(
    BackgroundJob.jobManager := { new CommandLineBackgroundJobManager() },
    Keys.onUnload := { s => try Keys.onUnload.value(s) finally BackgroundJob.jobManager.value.close() },
    BackgroundJob.jobList := { BackgroundJob.jobManager.value.list() },
    BackgroundJob.jobStop <<= jobStopTask(),
    BackgroundJob.jobWaitFor <<= jobWaitForTask())

  override val projectSettings = inConfig(Compile)(Seq(
    BackgroundJob.backgroundRunMain <<= backgroundRunMainTask(fullClasspath, runner in run),
    BackgroundJob.backgroundRun <<= backgroundRunTask(fullClasspath, mainClass in run, runner in run)))

  private def backgroundRunMainTask(classpath: Initialize[Task[Classpath]], scalaRun: Initialize[Task[ScalaRun]]): Initialize[InputTask[BackgroundJobHandle]] =
    {
      import DefaultParsers._
      val parser = Defaults.loadForParser(discoveredMainClasses)((s, names) => Defaults.runMainParser(s, names getOrElse Nil))
      Def.inputTask {
        val (mainClass, args) = parser.parsed
        BackgroundJob.jobManager.value.runInBackgroundThread(Keys.resolvedScoped.value, { (logger, uiContext) =>
          toError(scalaRun.value.run(mainClass, data(classpath.value), args, logger))
        })
      }
    }

  private def backgroundRunTask(classpath: Initialize[Task[Classpath]], mainClassTask: Initialize[Task[Option[String]]], scalaRun: Initialize[Task[ScalaRun]]): Initialize[InputTask[BackgroundJobHandle]] =
    {
      import Def.parserToInput
      import sys.error

      val parser = Def.spaceDelimited()
      Def.inputTask {
        val mainClass = mainClassTask.value getOrElse error("No main class detected.")
        BackgroundJob.jobManager.value.runInBackgroundThread(Keys.resolvedScoped.value, { (logger, uiContext) =>
          toError(scalaRun.value.run(mainClass, data(classpath.value), parser.parsed, logger))
        })
      }
    }

  private def jobIdParser: (State, Seq[BackgroundJobHandle]) => Parser[Seq[BackgroundJobHandle]] = {
    import DefaultParsers._
    (state, handles) => {
      val stringIdParser: Parser[Seq[String]] = Space ~> token(NotSpace examples handles.map(_.id.toString).toSet, description = "<job id>").+
      stringIdParser.map { strings =>
        strings.map(Integer.parseInt(_)).flatMap(id => handles.find(_.id == id))
      }
    }
  }

  // these three are copy-pasted from Defaults.scala to change them from
  // a static sbinary.Format to one generated from State
  private def loadFromContext[T](task: TaskKey[T], context: ScopedKey[_], s: State, fmt: State => sbinary.Format[T]): Option[T] =
    SessionVar.load(SessionVar.resolveContext(task.scopedKey, context.scope, s), s)(fmt(s))
  private def loadForParser[P, T](task: TaskKey[T], fmt: State => sbinary.Format[T])(f: (State, Option[T]) => Parser[P]): Initialize[State => Parser[P]] =
    loadForParserI(task, fmt)(Def value f)
  private def loadForParserI[P, T](task: TaskKey[T], fmt: State => sbinary.Format[T])(init: Initialize[(State, Option[T]) => Parser[P]]): Initialize[State => Parser[P]] =
    (resolvedScoped, init)((ctx, f) => (s: State) => f(s, loadFromContext(task, ctx, s, fmt)))

  private def foreachJobTask(f: (BackgroundJobManager, BackgroundJobHandle) => Unit): Initialize[InputTask[Unit]] = {
    import DefaultParsers._
    val formatGetter: State => sbinary.Format[Seq[BackgroundJobHandle]] = {
      s: State => seqFormat(Project.extract(s).get(BackgroundJob.jobManager).handleFormat)
    }
    val parser =
      loadForParser(BackgroundJob.jobList, formatGetter)((s, handles) => jobIdParser(s, handles getOrElse Nil))
    Def.inputTask {
      val handles = parser.parsed
      for (handle <- handles) {
        f(BackgroundJob.jobManager.value, handle)
      }
    }
  }

  private def jobStopTask(): Initialize[InputTask[Unit]] =
    foreachJobTask { (manager, handle) => manager.stop(handle) }

  private def jobWaitForTask(): Initialize[InputTask[Unit]] =
    foreachJobTask { (manager, handle) => manager.waitFor(handle) }
}
