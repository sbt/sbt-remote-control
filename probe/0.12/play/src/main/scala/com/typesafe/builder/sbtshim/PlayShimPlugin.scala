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

object PlayShimKeys {
  val playShimInstalled = SettingKey[Boolean]("play-shim-installed")

  val playShimRun = InputKey[Unit]("play-shim-run")

  val uiContext = com.typesafe.sbt.ui.SbtUiPlugin.uiContext
}

object PlayShimPlugin extends Plugin {
  import PlayShimKeys._

  override val settings: Seq[Setting[_]] = Seq(
    playShimInstalled := true,
    playShimRun <<= inputTask { (args: TaskKey[Seq[String]]) =>
      (args, state, uiContext in Global) map run
    })

  @volatile var stopped = false
  private val consoleReader = new jline.ConsoleReader
  def run(args: Seq[String], state: State, ctx: UIContext): Unit = {

    val extracted = Project.extract(state)

    // Parse HTTP port argument
    val port = extracted.get(playDefaultPort)

    println()

    val sbtLoader = this.getClass.getClassLoader
    def commonLoaderEither = Project.runTask(playCommonClassloader, state).get._2.toEither
    val commonLoader = commonLoaderEither.right.toOption.getOrElse {
      state.log.warn("some of the dependencies were not recompiled properly, so classloader is not avaialable")
      throw commonLoaderEither.left.get
    }
    val maybeNewState = Project.runTask(dependencyClasspath in Compile, state).get._2.toEither.right.map { dependencies =>

      // All jar dependencies. They will not been reloaded and must be part of this top classloader
      val classpath = dependencies.map(_.data.toURI.toURL).filter(_.toString.endsWith(".jar")).toArray

      /**
       * Create a temporary classloader to run the application.
       * This classloader share the minimal set of interface needed for
       * communication between SBT and Play.
       * It also uses the same Scala classLoader as SBT allowing to share any
       * values coming from the Scala library between both.
       */
      lazy val applicationLoader: ClassLoader = new java.net.URLClassLoader(classpath, commonLoader) {

        val sharedClasses = Seq(
          classOf[play.core.SBTLink].getName,
          classOf[play.core.server.ServerWithStop].getName,
          classOf[play.api.UsefulException].getName,
          classOf[play.api.PlayException].getName,
          classOf[play.api.PlayException.InterestingLines].getName,
          classOf[play.api.PlayException.RichDescription].getName,
          classOf[play.api.PlayException.ExceptionSource].getName,
          classOf[play.api.PlayException.ExceptionAttachment].getName)

        override def loadClass(name: String): Class[_] = {
          if (sharedClasses.contains(name)) {
            sbtLoader.loadClass(name)
          } else {
            super.loadClass(name)
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
          "SBT/Play shared ClassLoader, with: " + (getURLs.toSeq) + ", using parent: " + (getParent)
        }

      }

      lazy val reloader = newReloader(state, playReload, applicationLoader)

      val mainClass = applicationLoader.loadClass("play.core.server.NettyServer")
      val mainDev = mainClass.getMethod("mainDev", classOf[SBTLink], classOf[Int])

      // Run in DEV
      val server = mainDev.invoke(null, reloader, port: java.lang.Integer).asInstanceOf[play.core.server.ServerWithStop]

      // Notify hooks
      extracted.get(playOnStarted).foreach(_(server.mainAddress))
      ctx.sendEvent("playServerStarted", SimpleJsonMessage(JSONObject(Map("port" -> port))))

      println()
      println("(Server started, use Activator to stop....")
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
              println("[" + "success" + "] Compiled in " + formatted)
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
          consoleReader.getTerminal.disableEcho()
          executeContinuously(w, state, reloader)
          consoleReader.getTerminal.enableEcho()

          // Remove state two first commands added by sbt ~
          state.copy(remainingCommands = state.remainingCommands.drop(2)).remove(Watched.ContinuousState)
        }
        case _ => {
          // run mode
          @tailrec
          def blockForCancel(state: State): State = {
            ctx.take() match {
              case ui.NoUIPresent => state
              case ui.Canceled => state
              case ui.Request(name, handle, sendError) => {
                sendError("Request not supported during play run: " + name)
                blockForCancel(state)
              }
            }
          }
          blockForCancel(state)
        }
      }

      server.stop()
      reloader.clean()

      // Notify hooks
      extracted.get(playOnStopped).foreach(_())

      newState
    }

    println()

    maybeNewState match {
      case Right(x) => x
      case _ => state
    }
  }
}
