package sbt.server

import java.io.{ IOException, InputStream, OutputStream, PrintStream }
import java.util.concurrent.atomic.AtomicReference

import sbt.protocol

/** A trait representing something which can handle the standard input/output of the current JVM. */
trait StandardInputOuputHandler {
  /** Called to pass a set of characters from stderr out to the handler. */
  def stderr(bytes: String): Unit
  /** Called to pass a set of stdout characters out. */
  def stdout(bytes: String): Unit
  /** Called when we need to read the next set of bytes for standard input. */
  def stdin(buf: Array[Byte], off: Int, len: Int): Int
}
object StandardInputOuputHandler {
  object empty extends StandardInputOuputHandler {
    override def stderr(bytes: String): Unit = ()
    // TODO - this may need to pass in buf/len/off and return count
    override def stdin(buf: Array[Byte], off: Int, len: Int): Int = -1
    override def stdout(bytes: String): Unit = ()
    override def toString = "EmptyOutput"
  }
  object capturingOld extends StandardInputOuputHandler {
    val oldIn = System.in
    val oldOut = System.out
    val oldErr = System.err
    override def stderr(bytes: String): Unit = {
      oldErr.print(s"[err] $bytes")
      oldErr.flush()
    }
    override def stdin(buf: Array[Byte], off: Int, len: Int): Int = oldIn.read(buf, off, len)
    override def stdout(bytes: String): Unit = {
      oldOut.print(s"[out] $bytes")
      oldOut.flush()
    }
    override def toString = "InitialStdOutput"
  }
}

// annoyingly, to go PrintStream to Writer we need to go via an OutputStream because
// we need something for the PrintStream to wrap. Kind of ridiculous.
// Also inefficient.
private abstract class LineStream extends java.io.OutputStream {
  /** This will feed strings down, rather than raw bytes. */
  protected def writeLine(line: String): Unit
  import java.nio.charset._
  private val decoder = new ByteDecoder()
  private var count = 0
  private val MAX_COUNT = 1024
  // we ALWAYS get UTF-8, see how we set up the print streams below in takeoverSystemStreams.
  // Also the print stream is set to autoFlush so on every newline we should flush
  override def write(byte: Int): Unit = synchronized {
    decoder.feed(byte.toByte)
    // TODO - We also may want to just flush after a delay, if we can actually know that....
    count += 1
    if (count > MAX_COUNT) flush()
  }
  override def flush(): Unit = synchronized {
    decoder.read() foreach { s =>
      writeLine(s)
    }
    count = 0
  }
  override def close(): Unit = synchronized {
    decoder.finish()
    flush()
  }
}

/**
 * A shim which delegates responsibility of writing data out to a thread-local handler, or a gloabl.
 *
 *
 * This class can be `install`ed, which will override the default System.in/out/err streams.
 *
 * These streams will delegate to the registered `setGlobalHandler` for all threads.   Indiviudal threads
 * can override the flow by delegated to an `installLocalHandler`.
 */
object SystemOutShim {
  private val oldOut = System.out
  private val oldErr = System.err
  private val oldIn = System.in
  private[this] val global = new AtomicReference[StandardInputOuputHandler](StandardInputOuputHandler.capturingOld)
  private[this] val local = new ThreadLocal[StandardInputOuputHandler]
  private[this] def current: StandardInputOuputHandler = {
    val l = local.get
    if (l == null) global.get
    else l
  }
  def installLocalHandler(h: StandardInputOuputHandler): Unit = local.set(h)
  def uninstallLocalHandler(): Unit = local.remove()
  // TODO - Do we need to remove a global handler after install?
  def installGlobalHandler(h: StandardInputOuputHandler): Unit = global.lazySet(h)

  /** Shim to feed strings out the stdout stream. */
  private object OutLineStream extends LineStream {
    /** This will feed strings down, rather than raw bytes. */
    override protected def writeLine(line: String): Unit = current.stdout(line)
    override def close(): Unit = ()
  }
  /** Shim to feed stderr strings out to the stderr stream. */
  private object ErrLineStream extends LineStream {
    /** This will feed strings down, rather than raw bytes. */
    override protected def writeLine(line: String): Unit = current.stderr(line)
    override def close(): Unit = ()
  }
  /** Shim to feed InputStream events into the stin inteface. */
  private object InStream extends InputStream {
    override def read(): Int = {
      // TODO - More effiicent version of this....
      val b = new Array[Byte](1)
      read(b, 0, 1) match {
        case 1 => b(0)
        case -1 => -1
        case n => throw new IOException(s"Error, read $n bytes when attempting to only read 1")
      }
    }
    override def read(buf: Array[Byte], off: Int, len: Int): Int = current.stdin(buf, off, len)
    override def close(): Unit = ()
  }
  /** Updates the scala.Console references to the stdin/stdout mechanism. */
  private def refreshScalaConsole(): Unit = {
    scala.Console.setIn(System.in)
    scala.Console.setErr(System.err)
    scala.Console.setOut(System.out)
  }
  def install(): Unit = {
    System.setOut(new PrintStream(OutLineStream, true))
    System.setErr(new PrintStream(ErrLineStream, true))
    System.setIn(InStream)
    refreshScalaConsole()
  }

  def uninstall(): Unit = {
    System.setOut(oldOut)
    System.setErr(oldErr)
    System.setIn(oldIn)
    refreshScalaConsole()
  }

}
