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
/** Represents a value we can send over the wire. */
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
/** Represents a value we cannot send over the wire. */
case class UnserializedValue[T](stringValue: String) extends BuildValue[T] {
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
    defaultSerializers(mf) map { serializer =>
      SerializableBuildValue(o, serializer, TypeInfo.fromManifest(mf))
    } getOrElse UnserializedValue(o.toString)

    
  // TODO - This should be a registration system and not so hacky...
  /**
   * This represents the generic way in which we can serialize sbt settings over the network.
   * 
   * This is the ONLY list we use when attempting to inspect unknown types.  If we don't
   * have a mechanism here, we can't serialize (on either side) and we wind up with a
   * None representing the semantic value, but the "toString" will still make it across.
   */
  def defaultSerializers[T](mf: Manifest[T]): Option[Format[T]] = {
    (mf.erasure match {
      case Classes.StringClass => Some(implicitly[Format[String]])
      case Classes.FileClass => Some(implicitly[Format[java.io.File]])
      case Classes.BooleanClass => Some(implicitly[Format[Boolean]])
      case Classes.ShortClass => Some(implicitly[Format[Short]])
      case Classes.IntClass => Some(implicitly[Format[Int]])
      case Classes.LongClass => Some(implicitly[Format[Long]])
      case Classes.FloatClass => Some(implicitly[Format[Float]])
      case Classes.DoubleClass => Some(implicitly[Format[Double]])
      // TODO - polymorphism?
      case Classes.SeqSubClass() =>
        // Now we need to find the first type arguments structure:
        import collection.generic.CanBuildFrom
        for {
          child <- defaultSerializers(mf.typeArguments(0))
        } yield {
          val reads = Reads.traversableReads[Seq, Any](collection.breakOut, child.asInstanceOf[Reads[Any]])
          val writes = Writes.traversableWrites(child.asInstanceOf[Writes[Any]])
          Format(reads,writes)
        }
      case Classes.AttributedSubClass() =>
        for {
          child <- defaultSerializers(mf.typeArguments(0))
        } yield attributedFormat(child)
      case _ =>
        System.err.println("DEBUGME - Error:  No way to serialize: " + mf)
        None
    }).asInstanceOf[Option[Format[T]]]
  }
  
  private def deserialize(value: JsValue, mf: TypeInfo): Option[BuildValue[Any]] =
    for {
      realMf <- mf.toManifest()
      serializer <- defaultSerializers(realMf)
      realValue <- serializer.reads(value).asOpt
    } yield SerializableBuildValue[Any](realValue, serializer.asInstanceOf[Format[Any]], mf)
  
  // Hacky object so we don't instantiate classes just to satisfy typer.
  // We're safe at runtime given we ignore the type args after
  // erasure....
  private object MyRawFormat extends Format[BuildValue[Any]] {
    def writes(t: BuildValue[Any]): JsValue =
       t match {
         case UnserializedValue(string) =>
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
         fullOpt orElse Some(UnserializedValue(stringValue.toString))
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