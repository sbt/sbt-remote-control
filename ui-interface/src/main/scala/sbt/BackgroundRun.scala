package sbt

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
    BackgroundJob.jobList := { BackgroundJob.jobManager.value.list() })
  // TODO implement jobStop and jobWaitFor (requires writing a job ID parser)

  override val projectSettings = inConfig(Compile)(Seq(
    BackgroundJob.backgroundRunMain <<= backgroundRunMainTask(fullClasspath, runner in run),
    BackgroundJob.backgroundRun <<= backgroundRunTask(fullClasspath, mainClass in run, runner in run)))

  private def backgroundRunMainTask(classpath: Initialize[Task[Classpath]], scalaRun: Initialize[Task[ScalaRun]]): Initialize[InputTask[BackgroundJobHandle]] =
    {
      import DefaultParsers._
      val parser = loadForParser(discoveredMainClasses)((s, names) => Defaults.runMainParser(s, names getOrElse Nil))
      Def.inputTask {
        val (mainClass, args) = parser.parsed
        BackgroundJob.jobManager.value.runInBackgroundThread(streams.value, { (logger, uiContext) =>
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
        BackgroundJob.jobManager.value.runInBackgroundThread(streams.value, { (logger, uiContext) =>
          toError(scalaRun.value.run(mainClass, data(classpath.value), parser.parsed, logger))
        })
      }
    }
}
