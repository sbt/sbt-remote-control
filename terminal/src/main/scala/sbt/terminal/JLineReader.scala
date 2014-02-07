package sbt
package terminal

import java.io.File
import sbt.client.SbtClient
import sbt.client.Completion
import concurrent.{ Await, ExecutionContext }

// This class blocks on response from the server for autocompletion options.
final class RemoteJLineReader(
  historyPath: Option[File],
  client: SbtClient,
  val handleCONT: Boolean)(implicit ex: ExecutionContext) extends JLine {

  protected[this] val reader = {
    val r = JLine.createReader(historyPath)
    // TODO - Install completions.
    sbt.complete.JLineCompletion.installCustomCompletor(r)(blockingServerCompleter)
    r
  }

  private def blockingServerCompleter(line: String, level: Int): (Seq[String], Seq[String]) = {
    // TODO - configurable duration here..
    val timeout = concurrent.duration.Duration(2, java.util.concurrent.TimeUnit.SECONDS)
    Await.result(client.possibleAutocompletions(line, level).map(convertProtocolCompletions), timeout)
  }

  private def convertProtocolCompletions(cs: Set[Completion]): (Seq[String], Seq[String]) =
    if (cs.isEmpty) (Nil, "{invalid input}" :: Nil)
    else {
      import sbt.complete.JLineCompletion.appendNonEmpty
      val initial = Set.empty[String] -> Set.empty[String]
      val (insert, display) =
        (initial /: cs) {
          case (t @ (insert, display), comp) =>
            if (comp.isEmpty) t
            else (insert + comp.append, appendNonEmpty(display, comp.display))
        }
      (insert.toSeq, display.toSeq.sorted)
    }

}