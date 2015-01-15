package sbt.pickling

import scala.pickling._
import scala.reflect.runtime.universe.Mirror
import scala.util.{ Failure, Success }

object EmptyPickle extends Pickle {
  type ValueType = Unit
  val value: ValueType = ()
}

class DebugPickleFormat extends PickleFormat {
  type PickleType = EmptyPickle.type
  type OutputType = Output[String]
  def createBuilder() = new DebugPickleBuilder()
  def createBuilder(out: Output[String]): PBuilder = new DebugPickleBuilder()
  override def createReader(pickle: PickleType, mirror: Mirror) = {
    new DebugPickleReader()
  }
}

class DebugPickleBuilder(indent: Int = 0) extends PBuilder {
  private val indentString = (0 to indent) map (_ => "  ") mkString ""
  private def println(s: String): Unit = System.err.println(s"$indentString$s")
  private def nextLevelBuilder = new DebugPickleBuilder(indent + 1)

  override def beginEntry(picklee: Any): PBuilder = {
    println(s"beginEntry($picklee)")
    this
  }

  override def result(): Pickle = {
    println("result()")
    EmptyPickle
  }

  override def putElement(pickler: (PBuilder) => Unit): PBuilder = {
    println(s"putElement($pickler)")
    pickler(nextLevelBuilder)
    this
  }

  override def beginCollection(length: Int): PBuilder = {
    println(s"beginCollection($length)")
    this
  }

  override def endEntry(): Unit = {
    println("endEntry()")
  }

  override def endCollection(): Unit = {
    println(s"endCollection()")
  }

  override def putField(name: String, pickler: (PBuilder) => Unit): PBuilder = {
    println(s"putField($name, $pickler)")
    pickler(nextLevelBuilder)
    this
  }

  override def hintKnownSize(knownSize: Int): this.type = {
    println(s"hintKnownSize($knownSize")
    this
  }

  override def popHints(): this.type = {
    println(s"popHints()")
    this
  }

  override def pushHints(): this.type = {
    println(s"pushHints()")
    this
  }
  override def hintStaticallyElidedType(): this.type = {
    println(s"hintStaticallyElidedType()")
    this
  }

  override def hintOid(id: Int): this.type = {
    println(s"hintOid($id)")
    this
  }

  override def pinHints(): this.type = {
    println(s"pinHints()")
    this
  }

  override def hintTag(tag: FastTypeTag[_]): this.type = {
    println(s"hintTag($tag)")
    this
  }

  override def hintDynamicallyElidedType(): this.type = {
    System.err.println(s"hintDynamicallyElidedType()")
    this
  }

  override def unpinHints(): this.type = {
    System.err.println(s"unpinHints()")
    this
  }
}

class DebugPickleReader extends PReader with PickleTools {
  override def mirror: Mirror = null
  override def readPrimitive(): Any = ???
  override def beginCollection(): PReader = this
  override def readField(name: String): PReader = this
  override def endEntry(): Unit = ???
  override def atPrimitive: Boolean = ???
  override def readElement(): PReader = ???
  override def beginEntry(): FastTypeTag[_] = ???
  override def readLength(): Int = ???
  override def beginEntryNoTagDebug(debugOn: Boolean): String = ???
  override def atObject: Boolean = ???
  override def beginEntryNoTag(): String = ???
  override def endCollection(): Unit = ???
}