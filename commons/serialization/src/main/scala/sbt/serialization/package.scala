package sbt

import scala.util.control.NonFatal

package object serialization {
  import scala.pickling._

  // pickling macros need FastTypeTag$ to have been initialized;
  // if things ever compile with this removed, it can be removed.
  private val __forceInitializeFastTypeTagCompanion = FastTypeTag

  // this "newtypes" SPickler so we can require importing sbt.serialization._
  // and avoid the need to import scala.pickling.SPickler
  // FIXME maybe this should extend AnyVal but it was causing
  // "bridge generated for member method clashes with definition
  // of the member itself" errors
  /**
   * A pickler for sbt; wraps a scala.pickling.SPickler. When defining picklers,
   * define a scala.pickling.SPickler and let this be created implicitly.
   * When requiring a pickler for sbt purposes, require this type instead of
   * scala.pickling.SPickler directly.
   */
  final class SbtPickler[T] private[serialization] (val underlying: SPickler[T], val tag: FastTypeTag[T])

  // this is NOT in a companion object to help mandate import
  // of sbt.serialization._ to get our custom picklers
  implicit def sbtPicklerFromSPickler[T](implicit spickler: SPickler[T], tag: FastTypeTag[T]): SbtPickler[T] =
    new SbtPickler[T](spickler, tag)

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
    def apply[V](value: V)(implicit pickler: SbtPickler[V]): SerializedValue =
      JsonValue[V](value)(pickler)
  }

  private[sbt] sealed trait SbtPrivateSerializedValue extends SerializedValue {
    def toJson: JsonValue
  }

  /** A value we have serialized as JSON */
  private[sbt] final case class JsonValue(json: String /* TODO a non-string ast */ ) extends SbtPrivateSerializedValue {
    override def parse[T](implicit unpicklerForT: SbtUnpickler[T]): Option[T] = {
      implicit def ftt3: FastTypeTag[T] = ??? // FIXME remove and fix correctly
      implicit val u = unpicklerForT.underlying
      import scala.pickling.json.pickleFormat // TODO use our custom format
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
      implicit val t = picklerForT.tag
      import scala.pickling.json.pickleFormat // TODO use our custom format
      JsonValue(t.pickle.value)
    }

    val emptyObject = JsonValue("{}")
  }

  /**
   * A value we have the info available to serialize, but we haven't
   *  picked a format yet. Allows us to defer format selection.
   * TODO this confuses pickling because V is an AnyRef.
   */
  /*
  private[sbt] final case class LazyValue[V](value: V, pickler: SbtPickler[V]) extends SbtPrivateSerializedValue {
    // this could theoretically avoid the round-trip in some cases, but
    // pretty annoying to figure out what those cases are so forget
    // it. Not expecting to actually call this really anyway because
    // we use LazyValue on the "send" side.
    override def parse[T](implicit unpickler: SbtUnpickler[T]): Option[T] =
      toJson.parse[T]

    override def toJson = JsonValue(value)(pickler)
  }

  private[sbt] object LazyValue {
    // the macros can't generate a LazyValue pickler because it
    // contains a value of unknown (dynamic) type, but we can
    // create one by hand here that forces the LazyValue to be
    // evaluated to a JSON value.
    private val anyPickler = new SPickler[LazyValue[_]] with Unpickler[LazyValue[_]] {
      val jsonPickler = implicitly[SPickler[JsonValue]]
      val jsonUnpickler = implicitly[Unpickler[JsonValue]]
      override def pickle(lv: LazyValue[_], builder: PBuilder): Unit = {
        jsonPickler.pickle(lv.toJson, builder)
      }
      override def unpickle(tag: => FastTypeTag[_], preader: PReader): Any = {
        jsonUnpickler.unpickle(tag, preader)
      }
    }

    implicit def pickler[T]: SPickler[LazyValue[T]] =
      anyPickler.asInstanceOf[SPickler[LazyValue[T]]]

    implicit def unpickler[T]: Unpickler[LazyValue[T]] =
      anyPickler.asInstanceOf[Unpickler[LazyValue[T]]]
  }
 */

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
