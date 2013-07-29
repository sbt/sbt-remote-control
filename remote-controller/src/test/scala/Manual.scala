import com.typesafe.sbtrc._
import java.io.File
import akka.actor._
import akka.pattern._
import akka.dispatch._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout

object Main extends App {
  if (args.length < 1)
    throw new Exception("Specify directory")

  val system = ActorSystem("ManualTest")

  try {
    val child = SbtProcess(system, new File(args(0)), DebugSbtProcessLauncher)
    try {

      implicit val timeout = Timeout(60.seconds)

      val name = Await.result(child ? protocol.NameRequest(sendEvents = false), 60.seconds) match {
        case protocol.NameResponse(n) => {
          n
        }
        case protocol.ErrorResponse(error) =>
          throw new Exception("Failed to get project name: " + error)
      }
      println("Project is: " + name)

      val compiled = Await.result(child ? protocol.CompileRequest(sendEvents = false), 60.seconds) match {
        case protocol.CompileResponse(logs) => {
          System.err.println("logs=" + logs)
          true
        }
        case protocol.ErrorResponse(error) =>
          System.err.println("Failed to compile: " + error)
          false
      }

      println("compiled=" + compiled)

    } finally {
      system.stop(child)
    }
  } finally {
    system.shutdown()
    system.awaitTermination()
  }
}
