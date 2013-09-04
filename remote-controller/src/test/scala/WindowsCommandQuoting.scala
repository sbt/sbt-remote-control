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
    def q(s: String): String = SbtProcessLauncher.windowsQuoteCommandLine(Seq(s)).head

    for (t <- tests) {
      assertEquals(t._2, q(t._1))
    }
  }
}
