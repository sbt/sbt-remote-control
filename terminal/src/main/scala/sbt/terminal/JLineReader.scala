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

  private val reading = new java.util.concurrent.atomic.AtomicBoolean(false)

  // We synchronize output...
  def printLineAndRedrawPrompt(line: String): Unit = reader.synchronized {
    // TODO - Is this the correct mechanism?
    reader.println(s"\r$line")
    if (reading.get) reader.redrawLine()
  }

  override def readLine(prompt: String, mask: Option[Char]): Option[String] = {
    reading.set(true)
    try super.readLine(prompt, mask)
    finally reading.lazySet(false)
  }

  protected[this] val reader = {
    val r = JLine.createReader(historyPath)
    // TODO - Install custom completions if the client requests autocompletion...    
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