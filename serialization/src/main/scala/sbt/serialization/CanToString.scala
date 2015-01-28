package sbt.serialization

import java.io.File
import java.net.URI
// import xsbti.Severity
// import xsbti.Severity.{ Info, Warn, Error }

/**
 * This trait represents a type that can be converted to/from a human readable string.  The conversion MUST be
 * bi-directional.
 *
 * Note: There are Pickler helper methods to construct picklers which use this human-readable string when pickling
 *       rather than a more-binary-encoding of these types.   Ideal for messages which are sent/used in JSON.
 *
 * @tparam A  The type that can be human readable.
 */
trait CanToString[A] {
  def toString(a: A): String
  def fromString(s: String): A
}

/**
 * This contains default mechanisms to create CanToString and implciits for supported types: File, URI, TypeExpression.
 */
object CanToString {
  /**
   * Construct a new CanToString instance using the given conversion operations.
   *
   * NOTE:  The following must hold:  fs(ts(x)) == x
   *
   * @param ts  A function which can turn the type into a human readable string.
   * @param fs A function which can take the human readable string and turn it back into an instance of the same type.
   * @tparam A  The type we can conver.
   * @return  A new CanToString bidriectional conversion.
   */
  def apply[A](ts: A => String, fs: String => A): CanToString[A] = new CanToString[A] {
    def toString(a: A): String = ts(a)
    def fromString(s: String): A = fs(s)
  }
}
