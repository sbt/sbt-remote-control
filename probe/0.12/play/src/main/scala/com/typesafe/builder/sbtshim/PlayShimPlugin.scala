package com.typesafe.builder.sbtshim

import sbt._
import Keys._
import play.Project._
import sbt.InputTask
import sbt.InputTask
import collection.JavaConverters._
import annotation.tailrec
import play.core.SBTLink
import com.typesafe.sbt.ui
import com.typesafe.sbt.ui.{ Context => UIContext, SimpleJsonMessage }
import scala.util.parsing.json.JSONObject
import play.console.Colors

object PlayShimKeys {
  val playShimInstalled = SettingKey[Boolean]("play-shim-installed")
  val uiContext = com.typesafe.sbt.ui.SbtUiPlugin.uiContext
}

object PlayShimPlugin extends Plugin {
  import PlayShimKeys._

  override val settings: Seq[Setting[_]] = Seq(
    playShimInstalled := true,
    run in Compile <<= inputTask { (args: TaskKey[Seq[String]]) =>
      (args, state) map { (args, s) => runTask(s, args) }
    })

  @volatile var stopped = false
  private val consoleReader = new jline.ConsoleReader

  def runTask(
    state: State,
    args: Seq[String],
    runHooks: TaskKey[Seq[play.PlayRunHook]] = playRunHooks,
    dependencyClasspath: TaskKey[Classpath] = playDependencyClasspath,
    dependencyClassLoader: TaskKey[ClassLoaderCreator] = playDependencyClassLoader,
    reloaderClasspath: TaskKey[Classpath] = playReloaderClasspath,
    reloaderClassLoader: TaskKey[ClassLoaderCreator] = playReloaderClassLoader): Unit = {

    val extracted = Project.extract(state)

    val (_, hooks) = extracted.runTask(runHooks, state)

    val interaction = extracted.get(playInteractionMode)

    val (_, createClassLoader) = extracted.runTask(dependencyClassLoader, state)
    val (_, createReloader) = extracted.runTask(reloaderClassLoader, state)

    // Parse HTTP port argument
    //val (properties, port) = filterArgs(args, defaultPort = extracted.get(playDefaultPort))
    val port = extracted get playDefaultPort
    // Set Java properties
    //properties.foreach {
    //  case (key, value) => System.setProperty(key, value)
    //}

    println()

    val sbtLoader = this.getClass.getClassLoader
    def commonLoaderEither = Project.runTask(playCommonClassloader, state).get._2.toEither
    val commonLoader = commonLoaderEither.right.toOption.getOrElse {
      state.log.warn("some of the dependencies were not recompiled properly, so classloader is not avaialable")
      throw commonLoaderEither.left.get
    }
    val maybeNewState = Project.runTask(dependencyClasspath, state).get._2.toEither.right.map { dependencies =>

      // All jar dependencies. They will not been reloaded and must be part of this top classloader
      val classpath = Path.toURLs(dependencies.files)

      /**
       * Create a temporary classloader to run the application.
       * This classloader share the minimal set of interface needed for
       * communication between SBT and Play.
       * It also uses the same Scala classLoader as SBT allowing to share any
       * values coming from the Scala library between both.
       */
      lazy val delegatingLoader: ClassLoader = new ClassLoader(commonLoader) {

        val sharedClasses = Seq(
          classOf[play.core.SBTLink].getName,
          classOf[play.core.server.ServerWithStop].getName,
          classOf[play.api.UsefulException].getName,
          classOf[play.api.PlayException].getName,
          classOf[play.api.PlayException.InterestingLines].getName,
          classOf[play.api.PlayException.RichDescription].getName,
          classOf[play.api.PlayException.ExceptionSource].getName,
          classOf[play.api.PlayException.ExceptionAttachment].getName)

        override def loadClass(name: String, resolve: Boolean): Class[_] = {
          if (sharedClasses.contains(name)) {
            sbtLoader.loadClass(name)
          } else {
            super.loadClass(name, resolve)
          }
        }

        // -- Delegate resource loading. We have to hack here because the default implementation are already recursives.

        override def getResource(name: String): java.net.URL = {
          val findResource = classOf[ClassLoader].getDeclaredMethod("findResource", classOf[String])
          findResource.setAccessible(true)
          val resource = reloader.currentApplicationClassLoader.map(findResource.invoke(_, name).asInstanceOf[java.net.URL]).orNull
          if (resource == null) {
            super.getResource(name)
          } else {
            resource
          }
        }

        override def getResources(name: String): java.util.Enumeration[java.net.URL] = {
          val findResources = classOf[ClassLoader].getDeclaredMethod("findResources", classOf[String])
          findResources.setAccessible(true)
          val resources1 = reloader.currentApplicationClassLoader.map(findResources.invoke(_, name).asInstanceOf[java.util.Enumeration[java.net.URL]]).getOrElse(new java.util.Vector[java.net.URL]().elements)
          val resources2 = super.getResources(name)
          val resources = new java.util.Vector[java.net.URL](
            (resources1.asScala.toList ++ resources2.asScala.toList).distinct.asJava)
          resources.elements
        }

        override def toString = {
          "SBT/Play shared ClassLoader, using parent: " + (getParent)
        }

      }

      lazy val applicationLoader = createClassLoader("PlayDependencyClassLoader", classpath, delegatingLoader)

      lazy val reloader = newReloader(state, playReload, createReloader, reloaderClasspath, applicationLoader)

      // Now we're about to start, let's call the hooks:
      hooks.run(_.beforeStarted())

      val mainClass = applicationLoader.loadClass("play.core.server.NettyServer")
      val mainDev = mainClass.getMethod("mainDev", classOf[SBTLink], classOf[Int])

      // Run in DEV
      val server = mainDev.invoke(null, reloader, port: java.lang.Integer).asInstanceOf[play.core.server.ServerWithStop]

      // Notify hooks
      hooks.run(_.afterStarted(server.mainAddress))

      println()
      println(Colors.green("(Server started, use Ctrl+D to stop and go back to the console...)"))
      println()

      val ContinuousState = AttributeKey[WatchState]("watch state", "Internal: tracks state for continuous execution.")
      def isEOF(c: Int): Boolean = c == 4

      @tailrec def executeContinuously(watched: Watched, s: State, reloader: SBTLink, ws: Option[WatchState] = None): Option[String] = {
        @tailrec def shouldTerminate: Boolean = (System.in.available > 0) && (isEOF(System.in.read()) || shouldTerminate)

        val sourcesFinder = PathFinder { watched watchPaths s }
        val watchState = ws.getOrElse(s get ContinuousState getOrElse WatchState.empty)

        val (triggered, newWatchState, newState) =
          try {
            val (triggered, newWatchState) = SourceModificationWatch.watch(sourcesFinder, watched.pollInterval, watchState)(shouldTerminate)
            (triggered, newWatchState, s)
          } catch {
            case e: Exception =>
              val log = s.log
              log.error("Error occurred obtaining files to watch.  Terminating continuous execution...")
              (false, watchState, s.fail)
          }

        if (triggered) {
          //Then launch compile
          PlayProject.synchronized {
            val start = System.currentTimeMillis
            Project.runTask(compile in Compile, newState).get._2.toEither.right.map { _ =>
              val duration = System.currentTimeMillis - start
              val formatted = duration match {
                case ms if ms < 1000 => ms + "ms"
                case s => (s / 1000) + "s"
              }
              println("[" + Colors.green("success") + "] Compiled in " + formatted)
            }
          }

          // Avoid launching too much compilation
          Thread.sleep(Watched.PollDelayMillis)

          // Call back myself
          executeContinuously(watched, newState, reloader, Some(newWatchState))
        } else {
          // Stop 
          Some("Okay, i'm done")
        }
      }

      // If we have both Watched.Configuration and Watched.ContinuousState
      // attributes and if Watched.ContinuousState.count is 1 then we assume
      // we're in ~ run mode
      val maybeContinuous = state.get(Watched.Configuration).map { w =>
        state.get(Watched.ContinuousState).map { ws =>
          (ws.count == 1, w, ws)
        }.getOrElse((false, None, None))
      }.getOrElse((false, None, None))

      val newState = maybeContinuous match {
        case (true, w: sbt.Watched, ws) => {
          // ~ run mode
          interaction doWithoutEcho {
            executeContinuously(w, state, reloader)
          }

          // Remove state two first commands added by sbt ~
          state.copy(remainingCommands = state.remainingCommands.drop(2)).remove(Watched.ContinuousState)
        }
        case _ => {
          // run mode
          interaction.waitForCancel()
          state
        }
      }

      server.stop()
      reloader.clean()

      // Notify hooks
      hooks.run(_.afterStopped())

      newState
    }

    // Remove Java properties
    //properties.foreach {
    //  case (key, _) => System.clearProperty(key)
    //}

    println()

    maybeNewState match {
      case Right(x) => x
      case _ => state
    }
  }
}
