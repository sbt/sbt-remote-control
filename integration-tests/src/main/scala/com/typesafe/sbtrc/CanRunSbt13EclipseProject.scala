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
class CanRunSbt13EclipseProject extends SbtProcessLauncherTest {
  val dummy = utils.makeDummySbtProject("runChild13", TestUtil.sbt13TestVersion)
  val eclipsePluginFile = new File(dummy, "project/eclipse.sbt")
  sbt.IO.write(eclipsePluginFile,
    """
resolvers += Resolver.url("typesafe-ivy-releases", new URL("http://private-repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)      

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0")""")
  val child = SbtProcess(system, dummy, sbtProcessLauncher)
  try {
    Await.result(child ? ExecuteCommandRequest("eclipse", sendEvents = false), timeout.duration) match {
      case ExecuteCommandResponse() => // We succeeded!
      case whatever => throw new AssertionError("did not get RunResponse got " + whatever)
    }
    // Now we check to see if the eclipse files exist.
    def exists(name: String): Unit = {
      val file = new File(dummy, name)
      assert(file.exists, "Failed to generate file: " + file.getAbsolutePath)
    }
    exists(".classpath")
    exists(".project")
  } finally {
    system.stop(child)
  }
}
