package sbt
package client

/**
 * An interface which clients that support interaction with tasks implement.
 */
trait Interaction {
  /** Prompts the user for input, optionally with a mask for characters. */
  def readLine(prompt: String, mask: Boolean): Option[String]
  /** Ask the user to confirm something (yes or no) before continuing. */
  def confirm(msg: String): Boolean
}
