package sbt.protocol

import play.api.libs.json._
/**
 *  Represents a return value from attempting to pull a setting/value from sbt.
 * 
 * Some values are not serializable between JVMs.  This class ensures that libraries which
 * make use of core classes (like java.io.File, String, etc.) can return values.
 */
sealed trait BuildValue[T] {
  /** Return the value. */
  def value: Option[T]
  /** Result of calling toString on the value. */
  def stringValue: String
}
/** Represents a value we can send over the wire, both serializing + deserializing. */
case class SerializableBuildValue[T](
  rawValue: T,
  serializer: Format[T],
  manifest: TypeInfo
) extends BuildValue[T] {
  val value = Some(rawValue)
  val stringValue = rawValue.toString
  
  // We need to redo equality to ignore the serializer here.
  override def equals(o: Any): Boolean =
    o match {
      case x: SerializableBuildValue[_] => x.rawValue == rawValue
      case _ => false
    }  
  override def hashCode: Int = rawValue.hashCode  
  override def toString = "Serialized(with=" + serializer + ", toString=" + stringValue +")"
}
/** Represents a value we could not fully serialize over the wire.
 *  @param stringValue   The `toString` of the object we were trying to send.
 *  @param rawJson  If not None, this means the server knew how to serialize the value but we were
 *                  unable to decode it.   This JSON could still be used to introspect the data.  
 */
case class UnserializedValue[T](stringValue: String, rawJson: Option[JsValue]) extends BuildValue[T] {
  def value = None
}

/** Helper class lookups for serialization/deserialization. */
object Classes {
   val StringClass = classOf[String]
   val FileClass = classOf[java.io.File]
   val BooleanClass = classOf[Boolean]
   val ShortClass = classOf[Short]
   val IntClass = classOf[Int]
   val LongClass = classOf[Long]
   val FloatClass = classOf[Float]
   val DoubleClass = classOf[Double]
   val SeqClass = classOf[Seq[_]]
   val AttributedClass = classOf[sbt.Attributed[_]]
   
   // TODO - Figure out how to handle attributed, and
   // other sbt special classes....
   
   abstract class SubClass(cls: Class[_]) {
     def unapply(ocls: Class[_]): Boolean =
       cls.isAssignableFrom(ocls)
   }
   
   object SeqSubClass extends SubClass(SeqClass)
   object AttributedSubClass extends SubClass(AttributedClass)
}

// TODO - Clean up our serialziation of arbitrary values into RawStrcuture.... 
object BuildValue {
  
  // Here we need to reflectively look up the serialization of things...
  def apply[T](o: T)(implicit mf: Manifest[T]): BuildValue[T] = 
    DynamicSerialization.lookup(mf) map { serializer =>
      SerializableBuildValue(o, serializer, TypeInfo.fromManifest(mf))
    } getOrElse UnserializedValue(o.toString, None)



  private def deserialize(value: JsValue, mf: TypeInfo): Option[BuildValue[Any]] =
    for {
      realMf <- mf.toManifest()
      serializer <- DynamicSerialization.lookup(realMf)
      realValue <- serializer.reads(value).asOpt
    } yield SerializableBuildValue[Any](realValue, serializer.asInstanceOf[Format[Any]], mf)
  
  // Hacky object so we don't instantiate classes just to satisfy typer.
  // We're safe at runtime given we ignore the type args after
  // erasure....
  private object MyRawFormat extends Format[BuildValue[Any]] {
    def writes(t: BuildValue[Any]): JsValue =
       t match {
         case UnserializedValue(string, _) =>
           JsObject(Seq("stringValue" -> JsString(string)))
         case SerializableBuildValue(value, serializer, mf) =>
           JsObject(Seq(
              "stringValue" -> JsString(value.toString),
               "manifest" -> Json.toJson(mf),
               "value" -> serializer.writes(value)))
       }
     def reads(map: JsValue): JsResult[BuildValue[Any]] = {
       (map \ "stringValue").asOpt[String].flatMap { stringValue =>
         // TODO - Check for additional deserializers...
         val fullOpt: Option[BuildValue[Any]] = 
           for {
             mf <- (map \ "manifest").asOpt[TypeInfo]
             result <- deserialize((map \ "value"), mf)
           } yield result
         fullOpt orElse Some(UnserializedValue(stringValue.toString, Some(map \ "value")))
       } match {
        case Some(result) => JsSuccess(result.asInstanceOf[BuildValue[Any]])
        case None => JsError("Could not resolve build value!")
       }
     }
     override def toString = "RawBuildValueFormat"
  }
  implicit def MyFormat[T]: Format[BuildValue[T]] = MyRawFormat.asInstanceOf[Format[BuildValue[T]]]
  
  
  // Default handlers....
  
  
}


/** represents potential results coming back from an sbt SettingValueRequest, TaskValueRequest or
 *  InputTaskValueRequest.
 */
sealed trait TaskResult[T] {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean
}
/** This represents that the task was run successfully. */
case class TaskSuccess[T](value: BuildValue[T]) extends TaskResult[T] {
  override def isSuccess = true
}
/** This represents that there was an error running a task, and returns the error message. */
case class TaskFailure[T](message: String) extends TaskResult[T] {
  override def isSuccess = false
}

object TaskResult {
  implicit def Format[T](implicit p: Format[BuildValue[T]]): Format[TaskResult[T]] =
    new Format[TaskResult[T]] {
      def writes(t: TaskResult[T]): JsValue = 
        t match {
          case TaskFailure(msg) => JsObject(Seq("success" -> JsBoolean(false), "message" -> JsString(msg)))
          case TaskSuccess(value) => JsObject(Seq("success" -> JsBoolean(true), "value" -> Json.toJson(value)))
        }
      def reads(m: JsValue): JsResult[TaskResult[T]] = {
        (m \ "success") match {
          case JsBoolean(true) => 
            p.reads(m \ "value").map(TaskSuccess.apply)
          case JsBoolean(false) =>
            JsSuccess(TaskFailure((m \ "message").as[String]))
          case _ => 
            JsError("Unable to deserialize task result.")
        }
      }
      override def toString = "TaskResultFormat(" + p + ")"
    }
}