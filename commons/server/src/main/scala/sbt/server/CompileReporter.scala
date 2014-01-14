package sbt
package server

import xsbti.Position
import xsbti.Severity


class CompileReporter(
    // TODO - Sbt - client and more...
    maximumErrors: Int, 
    log: Logger, 
    sourcePositionMapper: Position => Position = {p => p}) 
  extends LoggerReporter(maximumErrors, log, sourcePositionMapper) {
  
  override def reset(): Unit =  {
    // TODO - Fire event to listeners that errors are reset.
    super.reset()
  }
  
  override def display(pos: Position, msg: String, severity: Severity) {
    // TODO - Fire the event here.
	super.display(pos,msg,severity)	
  }
  
  override def printSummary(): Unit = {
    // TODO - Fire event that summary is complete.
    super.printSummary();
  }
}
object CompileReporter {
  
  def makeShims(state: State): Seq[Setting[_]] = {
    // TODO - Override the derived compiler settings such that
    // our listener is installed on all compilers.
    Seq.empty
  }
}
