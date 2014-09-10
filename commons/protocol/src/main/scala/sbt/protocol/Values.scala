package sbt.protocol

import play.api.libs.json._
/**
 *  Represents a return value from attempting to pull a setting/value from sbt.
 *
 * Some values are not serializable between JVMs.  This class ensures that libraries which
 * make use of core classes (like java.io.File, String, etc.) can return values.
 */
sealed trait BuildValue[+T] {
  /** Return the value. */
  def value: Option[T]
  /** Result of calling toString on the value. */
  def stringValue: String
}
/** Represents a value we can send over the wire, both serializing + deserializing. */
final case class SerializableBuildValue[T](
  rawValue: T,
  serializer: Writes[T],
  manifest: TypeInfo) extends BuildValue[T] {
  val value = Some(rawValue)
  val stringValue = rawValue.toString

  // We need to redo equality to ignore the serializer here.
  override def equals(o: Any): Boolean =
    o match {
      case x: SerializableBuildValue[_] => x.rawValue == rawValue
      case _ => false
    }
  override def hashCode: Int = rawValue.hashCode
  override def toString = "Serialized(with=" + serializer + ", toString=" + stringValue + ")"
}
/**
 * Represents a value we could not fully serialize over the wire.
 *  @param stringValue   The `toString` of the object we were trying to send.
 *  @param rawJson  If not None, this means the server knew how to serialize the value but we were
 *                  unable to decode it.   This JSON could still be used to introspect the data.
 */
final case class UnserializedValue[T](stringValue: String, rawJson: Option[JsValue]) extends BuildValue[T] {
  def value = None
}

/** Helper class lookups for serialization/deserialization. */
private[protocol] object Classes {
  val StringClass = classOf[String]
  val FileClass = classOf[java.io.File]
  val BooleanClass = classOf[Boolean]
  val ShortClass = classOf[Short]
  val IntClass = classOf[Int]
  val LongClass = classOf[Long]
  val FloatClass = classOf[Float]
  val DoubleClass = classOf[Double]
  val OptionClass = classOf[Option[_]]
  val SeqClass = classOf[Seq[_]]
  val AttributedClass = classOf[sbt.Attributed[_]]
  val URIClass = classOf[java.net.URI]

  // TODO - Figure out how to handle attributed, and
  // other sbt special classes....

  abstract class SubClass(cls: Class[_]) {
    def unapply(ocls: Class[_]): Boolean =
      cls.isAssignableFrom(ocls)
  }

  object OptionSubClass extends SubClass(OptionClass)
  object SeqSubClass extends SubClass(SeqClass)
  object AttributedSubClass extends SubClass(AttributedClass)
}

// TODO - Clean up our serialziation of arbitrary values into RawStrcuture.... 
object BuildValue {

  // Here we need to reflectively look up the serialization of things...
  def apply[T](o: T, serializations: DynamicSerialization)(implicit mf: Manifest[T]): BuildValue[T] =
    serializations.lookup(mf) map { serializer =>
      SerializableBuildValue(o, serializer, TypeInfo.fromManifest(mf))
    } getOrElse UnserializedValue(o.toString, None)

  private def deserialize(value: JsValue, mf: TypeInfo, serializations: DynamicSerialization): Option[BuildValue[Any]] =
    for {
      realMf <- mf.toManifest()
      serializer <- serializations.lookup(realMf)
      realValue <- serializer.reads(value).asOpt
    } yield SerializableBuildValue[Any](realValue, serializer.asInstanceOf[Format[Any]], mf)

  private class BuildValueReads(val serializations: DynamicSerialization) extends Reads[BuildValue[Any]] {
    def reads(map: JsValue): JsResult[BuildValue[Any]] = {
      (map \ "stringValue").asOpt[String].flatMap { stringValue =>
        // TODO - Check for additional deserializers...
        val fullOpt: Option[BuildValue[Any]] =
          for {
            mf <- (map \ "manifest").asOpt[TypeInfo]
            result <- deserialize((map \ "value"), mf, serializations)
          } yield result
        fullOpt orElse Some(UnserializedValue(stringValue.toString, Some(map \ "value")))
      } match {
        case Some(result) => JsSuccess(result.asInstanceOf[BuildValue[Any]])
        case None => JsError("Could not resolve build value!")
      }
    }
  }

  // this one does NOT require a DynamicSerialization to be passed in
  // so it's available statically
  private object buildValueWrites extends Writes[BuildValue[Any]] {
    override def writes(t: BuildValue[Any]): JsValue =
      t match {
        case UnserializedValue(string, _) =>
          JsObject(Seq("stringValue" -> JsString(string)))
        case SerializableBuildValue(value, serializer, mf) =>
          JsObject(Seq(
            "stringValue" -> JsString(value.toString),
            "manifest" -> Json.toJson(mf),
            "value" -> serializer.writes(value)))
      }
  }

  // Hacky object so we don't instantiate classes just to satisfy typer.
  // We're safe at runtime given we ignore the type args after
  // erasure....
  private class BuildValueFormat(serializations: DynamicSerialization) extends Format[BuildValue[Any]] {
    override def writes(t: BuildValue[Any]): JsValue =
      buildValueWrites.writes(t)

    val reader = new BuildValueReads(serializations)

    override def reads(map: JsValue): JsResult[BuildValue[Any]] =
      reader.reads(map)

    override def toString = "RawBuildValueFormat"
  }

  // we usually use the same serializations for a bunch of values at once
  // so just keep around this object
  @volatile
  private var oneItemCache: Option[BuildValueFormat] = None

  private def getFormat(serializations: DynamicSerialization): BuildValueFormat = {
    oneItemCache.filter(fmt => fmt.reader.serializations eq serializations).getOrElse {
      val created = new BuildValueFormat(serializations)
      // tasty side effects
      oneItemCache = Some(created)
      created
    }
  }

  def format[T](serializations: DynamicSerialization): Format[BuildValue[T]] =
    getFormat(serializations).asInstanceOf[Format[BuildValue[T]]]

  def reads[T](serializations: DynamicSerialization): Reads[BuildValue[T]] =
    getFormat(serializations).reader.asInstanceOf[Reads[BuildValue[T]]]

  implicit def writes[T]: Writes[BuildValue[T]] =
    buildValueWrites.asInstanceOf[Writes[BuildValue[T]]]

  // Default handlers....

}

/**
 * represents potential results coming back from an sbt SettingValueRequest, TaskValueRequest or
 *  InputTaskValueRequest.
 */
sealed trait TaskResult[T] {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean
}
/** This represents that the task was run successfully. */
final case class TaskSuccess[T](value: BuildValue[T]) extends TaskResult[T] {
  override def isSuccess = true
}
/** This represents that there was an error running a task, and returns the error message. */
final case class TaskFailure[T](message: String) extends TaskResult[T] {
  override def isSuccess = false
}

object TaskResult {
  implicit def reads[T](implicit p: Reads[BuildValue[T]]): Reads[TaskResult[T]] =
    new Reads[TaskResult[T]] {
      override def reads(m: JsValue): JsResult[TaskResult[T]] = {
        (m \ "success") match {
          case JsBoolean(true) =>
            p.reads(m).map(TaskSuccess.apply)
          case JsBoolean(false) =>
            JsSuccess(TaskFailure((m \ "message").as[String]))
          case _ =>
            JsError("Unable to deserialize task result.")
        }
      }
    }
  implicit def writes[T](implicit p: Writes[BuildValue[T]]): Writes[TaskResult[T]] =
    new Writes[TaskResult[T]] {
      override def writes(t: TaskResult[T]): JsValue =
        t match {
          case TaskFailure(msg) => JsObject(Seq("success" -> JsBoolean(false), "message" -> JsString(msg)))
          case TaskSuccess(value) =>
            val base = Json.obj("success" -> true)
            val valueJson = Json.toJson(value) match {
              case o: JsObject => o
              case other => throw new RuntimeException("serialized a BuildValue to non-JsObject")
            }
            base ++ valueJson
        }
    }
  implicit def format[T](implicit p: Format[BuildValue[T]]): Format[TaskResult[T]] =
    new Format[TaskResult[T]] {
      val reader = TaskResult.reads[T]
      val writer = TaskResult.writes[T]
      def writes(t: TaskResult[T]): JsValue =
        writer.writes(t)
      def reads(m: JsValue): JsResult[TaskResult[T]] =
        reader.reads(m)
      override def toString = "TaskResultFormat(" + p + ")"
    }
}
