package sbt

import scala.util.control.NonFatal

package object serialization {
  import scala.pickling._

  /* workaround for https://github.com/scala/pickling/issues/227 */
  implicit def fastTypeTag[T]: FastTypeTag[T] = {
    FastTypeTag.materializeFastTypeTag[T]
  }

  // FIXME this is a workaround for requiring a format
  // to generate a pickler; we delete this once we update
  // to newer pickling.
  implicit def formatHack = scala.pickling.json.pickleFormat

  // this "newtypes" SPickler so we can require importing sbt.serialization._
  // FIXME maybe this should extend AnyVal but it was causing
  // "bridge generated for member method clashes with definition
  // of the member itself" errors
  /**
   * A pickler for sbt; wraps a scala.pickling.SPickler. When defining picklers,
   * define a scala.pickling.SPickler and let this be created implicitly.
   * When requiring a pickler for sbt purposes, require this type instead of
   * scala.pickling.SPickler directly.
   */
  final class SbtPickler[T] private[serialization] (val underlying: SPickler[T])

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtPicklerFromSPickler[T](implicit spickler: SPickler[T]): SbtPickler[T] =
    new SbtPickler[T](spickler)

  // FIXME maybe this should extend AnyVal but it was causing
  // "bridge generated for member method clashes with definition
  // of the member itself" errors
  /**
   * An unpickler for sbt; wraps a scala.pickling.Unpickler. When defining unpicklers,
   * define a scala.pickling.Unpickler and let this be created implicitly.
   * When requiring an unpickler for sbt purposes, require this type instead of
   * scala.pickling.Unpickler directly.
   */
  final class SbtUnpickler[T] private[serialization] (val underlying: Unpickler[T])

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtUnpicklerFromUnpickler[T](implicit unpickler: Unpickler[T]): SbtUnpickler[T] =
    new SbtUnpickler[T](unpickler)

  /**
   * A pair of pickler and unpickler; don't define this by hand, just let
   * it be implicitly created when you define a scala.pickling.SPickler and
   * scala.pickling.Unpickler (or use the default definitions for those).
   */
  final class SbtSerializer[T] private[serialization] (val pickler: SbtPickler[T], val unpickler: SbtUnpickler[T])

  object SbtSerializer {
    def apply[T](pickler: SbtPickler[T], unpickler: SbtUnpickler[T]): SbtSerializer[T] =
      new SbtSerializer[T](pickler, unpickler)
  }

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtSerializerFromPicklerAndUnpickler[T](implicit pickler: SbtPickler[T], unpickler: SbtUnpickler[T]): SbtSerializer[T] =
    SbtSerializer[T](pickler, unpickler)

  /**
   * We serialize to and from this opaque type. The idea is to
   * hide exactly what we can serialize from/to and hide
   * which library we use to do it.
   */
  sealed trait SerializedValue {
    def parse[T](implicit unpickler: SbtUnpickler[T]): Option[T]
  }

  object SerializedValue {
    def apply[T](t: T)(implicit pickler: SbtPickler[T]): SerializedValue =
      LazyValue[T](t, pickler)
  }

  private[sbt] sealed trait SbtPrivateSerializedValue extends SerializedValue {
    def toJson: JsonValue
  }

  /** A value we have serialized as JSON */
  private[sbt] final case class JsonValue(json: String /* TODO a non-string ast */ ) extends SbtPrivateSerializedValue {
    override def parse[T](implicit unpicklerForT: SbtUnpickler[T]): Option[T] = {
      implicit val u = unpicklerForT.underlying
      // uncomment when formatHack removed above
      //import scala.pickling.json.pickleFormat // TODO use our custom format
      try Some(scala.pickling.json.JSONPickle(json).unpickle[T])
      catch {
        // TODO we drop any detailed error message the pickler may have offered.
        // Make read return a Try?
        case NonFatal(e) => None
      }
    }
    override def toJson = this
  }

  private[sbt] object JsonValue {
    def apply[T](t: T)(implicit picklerForT: SbtPickler[T]): JsonValue = {
      implicit val p = picklerForT.underlying
      // uncomment when formatHack removed above
      //import scala.pickling.json.pickleFormat // TODO use our custom format
      JsonValue(t.pickle.value)
    }

    val emptyObject = JsonValue("{}")
  }

  /**
   * A value we have the info available to serialize, but we haven't
   *  picked a format yet. Allows us to defer format selection.
   */
  private[sbt] final case class LazyValue[V](value: V, pickler: SbtPickler[V]) extends SbtPrivateSerializedValue {
    // this could theoretically avoid the round-trip in some cases, but
    // pretty annoying to figure out what those cases are so forget
    // it. Not expecting to actually call this really anyway because
    // we use LazyValue on the "send" side.
    override def parse[T](implicit unpickler: SbtUnpickler[T]): Option[T] =
      toJson.parse[T]

    override def toJson = JsonValue(value)(pickler)
  }

  // hypothetical to show why we have the SerializedValue hierarchy - not currently used
  //private[sbt] final case class BinaryValue(bytes: Array[Byte]) extends SerializedValue

  def fileFromString(s: String): Option[java.io.File] =
    try Some(new java.io.File(new java.net.URI(s)))
    catch {
      case e: Exception => None
    }
  def fileToString(f: java.io.File): String =
    f.toURI.toASCIIString

  private def readString(tag: FastTypeTag[_], reader: PReader): String = {
    val stringUnpickler = implicitly[Unpickler[String]]
    stringUnpickler.unpickle(tag, reader).asInstanceOf[String]
  }

  private final class AsStringPickler[T](f: T => String) extends SPickler[T] {
    override def pickle(t: T, builder: PBuilder): Unit = {
      val stringPickler = implicitly[SPickler[String]]
      stringPickler.pickle(f(t), builder)
    }
  }

  private object AsStringPickler {
    def apply[T](f: T => String): SPickler[T] = new AsStringPickler(f)
  }

  private final class AsStringUnpickler[T](f: String => T) extends Unpickler[T] {
    override def unpickle(tag: => FastTypeTag[_], reader: PReader): T = {
      f(readString(tag, reader))
    }
  }

  private object AsStringUnpickler {
    def apply[T](f: String => T): Unpickler[T] = new AsStringUnpickler(f)
  }

  implicit val fileUnpickler = AsStringUnpickler[java.io.File] { filename =>
    fileFromString(filename).getOrElse(throw PicklingException(s"Invalid filename $filename"))
  }
  implicit val filePickler = AsStringPickler[java.io.File] { file =>
    fileToString(file)
  }

  implicit val uriUnpickler = AsStringUnpickler[java.net.URI] { uriString =>
    try new java.net.URI(uriString)
    catch {
      case NonFatal(e) => throw PicklingException(s"Invalid URI $uriString" /* FIXME , e */ )
    }
  }

  implicit val uriPickler = AsStringPickler[java.net.URI] { uri => uri.toASCIIString }

  private final class Funpickler[T](fun: (FastTypeTag[_], PReader) => T) extends Unpickler[T] {
    override def unpickle(tag: => FastTypeTag[_], reader: PReader): T = fun(tag, reader)
  }

  private object Funpickler {
    def apply[T](fun: (FastTypeTag[_], PReader) => T): Unpickler[T] = new Funpickler[T](fun)
  }

  // this is not implicit because it would cause trouble with
  // more specific serializers; we just use it as an explicit fallback
  val throwableUnpickler = Funpickler[java.lang.Throwable] { (tag, reader) =>
    /*
    implicit def recursiveUnpickler = throwableUnpickler
    val messageReader = reader.readField("message")
    val message = readString(FastTypeTag.String, messageReader)
    val causeReader = reader.readField("cause")
    val cause = throwableReads.read()
    def validateOrNull[T <: AnyRef](json: JsValue)(implicit r: Reads[T]): JsResult[T] = json match {
      case JsNull => JsSuccess(null.asInstanceOf[T])
      case _ => r.reads(json)
    }
    for {
      message <- validateOrNull[String](v \ "message")
      cause <- validateOrNull[Throwable](v \ "cause")
    } yield new Exception(message, cause)

    */
    ???
  }
  val throwablePickler: SPickler[java.lang.Throwable] = ??? /*{ t =>
    implicit def recursivePickler = throwableSPickler
    JsObject(Seq("message" -> Option(t.getMessage).map(JsString(_)).getOrElse(JsNull),
      "cause" -> Option(t.getCause).map(Json.toJson(_)).getOrElse(JsNull)))
     ???
  }
  *
  */
}
