package com.typesafe.sbtrc

import com.typesafe.sbtrc.protocol._
import com.typesafe.sbtrc.it._
import java.io.File
import akka.actor._
import akka.pattern._
import akka.dispatch._
import concurrent.duration._
import concurrent.Await
import akka.util.Timeout
import akka.pattern.ask

/** Ensures that we can make requests and receive responses from our children. */
class CanRunSbt13IdeaProject extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProject("runChild13-idea", "0.13.0")
  val ideaPluginFile = new File(dummy, "project/idea.sbt")
  sbt.IO.write(ideaPluginFile,
    """addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.1")""")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  try {
    Await.result(child ? GenericRequest(name = "gen-idea", sendEvents = false, params = Map.empty), timeout.duration) match {
      case GenericResponse("gen-idea", _) => // We succeeded!
      case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
    }
    // Now we check to see if the eclipse files exist.
    def exists(name: String): Unit = {
      val file = new File(dummy, name)
      assert(file.exists, "Failed to generate file: " + file.getAbsolutePath)
    }
    exists(".idea")
  } finally {
    system.stop(child)
  }
}
