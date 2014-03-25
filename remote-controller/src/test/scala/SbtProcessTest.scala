/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc._
import com.typesafe.sbtrc.protocol._
import java.io.File
import akka.actor._
import akka.dispatch._
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern._
import akka.util.Timeout
import com.typesafe.sbtrc.launching.SbtProcessLauncher

class SbtProcessTest {

  // this is ANNOYINGLY long if you're debugging tests by hand, but
  // we don't want it to keep randomly failing.
  // just edit it shorter locally if you are hitting it when coding.
  implicit val timeout = Timeout(300.seconds)

  object EchoHelloChildProcessMaker extends SbtProcessLauncher {

    private def isWindows =
      sys.props("os.name").toLowerCase contains "windows"

    override def apply(cwd: File, port: Int, extraJvmArgs: Seq[String] = Seq.empty[String]): ProcessBuilder = {
      val cmd =
        if (isWindows) Seq("cmd.exe", "/c", "echo", "Hello World")
        else Seq("echo", "Hello World")
      import collection.JavaConverters._
      (new ProcessBuilder).command(cmd.asJava).directory(cwd)
    }
  }

  @Test
  def testChildProcessNeverConnects(): Unit = {
    val system = ActorSystem("test-child-never-connects")

    val child = SbtProcess(system, new File("."), EchoHelloChildProcessMaker)

    // the test is that the child should die on its own so actor system shutdown should work

    system.shutdown()
    system.awaitTermination()
  }

}
