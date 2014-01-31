package sbt
package interaction


/** A mechanism of servicing autocompletion requests when asking for more input.
 * 
 */
trait CompletionHandler {
  def possibleAutocompletions(partialCommand: String, detailLevel: Int): Set[Completion]
}

/**
* Represents a completion.
* The abstract members `display` and `append` are best explained with an example. 
*
* Assuming space-delimited tokens, processing this:
*   am is are w<TAB>
* could produce these Completions:
*   Completion { display = "was"; append = "as" }
*   Completion { display = "were"; append = "ere" }
* to suggest the tokens "was" and "were".
*
* In this way, two pieces of information are preserved:
*  1) what needs to be appended to the current input if a completion is selected
*  2) the full token being completed, which is useful for presenting a user with choices to select
*/
final case class Completion(append: String, display: String) {
  def isEmpty: Boolean = append == null || append.isEmpty
}