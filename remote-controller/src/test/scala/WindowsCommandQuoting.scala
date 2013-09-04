/**
 *   Copyright (C) 2012 Typesafe Inc. <http://typesafe.com>
 */
import org.junit.Assert._
import org.junit._
import com.typesafe.sbtrc.launching.SbtProcessLauncher

class WindowsCommandQuoting {

  @Test
  def testQuoting(): Unit = {
    val tests = Map("" -> "\"\"",
      " " -> "\" \"",
      "foo bar" -> "\"foo bar\"",
      "\"" -> """"\""""",
      """\""" -> """"\\"""",
      """\\\""" -> """"\\\\\\"""")

    // the first item in the sequence is special (doesn't get quoted)
    // so we grab the second one for testing this
    def q(s: String): String = SbtProcessLauncher.windowsQuoteCommandLine(Seq("command", s)).tail.head

    for (t <- tests) {
      assertEquals(t._2, q(t._1))
    }
  }
}
