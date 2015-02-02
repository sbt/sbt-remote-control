package sbt

import sbt.serialization._

package object protocol {
  import sbt.serialization.CanToString

  private implicit val severityCanToString: CanToString[xsbti.Severity] = CanToString(
    _.toString, {
      case "Info" => xsbti.Severity.Info
      case "Warn" => xsbti.Severity.Warn
      case "Error" => xsbti.Severity.Error
    })

  implicit val severityPickler: SPickler[xsbti.Severity] with Unpickler[xsbti.Severity] =
    canToStringPickler[xsbti.Severity]

  private def convert[T](o: Option[T]): xsbti.Maybe[T] =
    o match {
      case Some(value) => xsbti.Maybe.just(value)
      case None => xsbti.Maybe.nothing()
    }

  private def convertToOption[T](o: xsbti.Maybe[T]): Option[T] =
    if (o.isDefined()) Some(o.get())
    else None

  // TODO there's no real logic to why these are here and others are in
  // companion objects.
  implicit val positionPickler = genPickler[Position]
  implicit val positionUnpickler = genUnpickler[Position]
  implicit val compilationFailurePickler = genPickler[CompilationFailure]
  implicit val compilationFailureUnpickler = genUnpickler[CompilationFailure]
  implicit val moduleIdPickler = genPickler[ModuleId]
  implicit object moduleIdUnpickler extends Unpickler[ModuleId] {
    private val stringUnpickler = sbt.serialization.stringPickler
    private val attrsUnpickler = sbt.serialization.stringMapPickler[String]
    override def unpickle(tag: String, reader: PReader): Any = {
      reader.pushHints()
      reader.hintTag(this.tag)
      reader.hintStaticallyElidedType()
      // TODO - this was already called...
      reader.beginEntry()
      def readString(field: String): String = {
        stringUnpickler.unpickleEntry(reader.readField(field)).toString
      }
      val org = readString("organization")
      val n = readString("name")
      val attrs = {
        val sr = reader.readField("attributes")
        sr.hintTag(attrsUnpickler.tag)
        sr.hintStaticallyElidedType()
        attrsUnpickler.unpickle(attrsUnpickler.tag.key, sr).asInstanceOf[Map[String, String]]
      }
      reader.endEntry()
      reader.popHints()
      ModuleId(org, n, attrs)
    }
    override val tag: FastTypeTag[ModuleId] = implicitly[FastTypeTag[ModuleId]]
  }
}
