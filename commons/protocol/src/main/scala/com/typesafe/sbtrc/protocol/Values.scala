package com.typesafe.sbtrc
package protocol


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
  serializer: RawStructure[T],
  manifest: TypeInfo
) extends BuildValue[T] {
  val value = Some(rawValue)
  val stringValue = rawValue.toString
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

// TODO - Figure out how to serialize arbitrary values using Parametizable 
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
  def defaultSerializers[T](mf: Manifest[T]): Option[RawStructure[T]] = {
    (mf.erasure match {
      case Classes.StringClass => Some(RawStructure.get[String])
      case Classes.FileClass => Some(RawStructure.get[java.io.File])
      case Classes.BooleanClass => Some(RawStructure.get[Boolean])
      // TODO - polymorphism?
      case Classes.SeqSubClass() =>
        // Now we need to find the first type arguments structure:
        for {
          child <- defaultSerializers(mf.typeArguments(0))
        } yield RawStructure.SeqStructure(child)
      case Classes.AttributedSubClass() =>
        for {
          child <- defaultSerializers(mf.typeArguments(0))
        } yield RawStructure.AttributedStructure(child)
      case _ =>
        println("Error:  No way to serialize: " + mf)
        None
    }).asInstanceOf[Option[RawStructure[T]]]
  }
  
  private def deserialize(value: Map[String,Any], mf: TypeInfo): Option[BuildValue[Any]] =
    for {
      realMf <- mf.toManifest()
      serializer <- defaultSerializers(realMf)
      realValue <- serializer.unapply(value)
    } yield SerializableBuildValue[Any](realValue, serializer.asInstanceOf[RawStructure[Any]], mf)
  
  // Hacky object so we don't instantiate classes just to satisfy typer.
  // We're safe at runtime given we ignore the type args after
  // erasure....
  private object MyRawStructure extends protocol.RawStructure[BuildValue[Any]] {
    def apply(t: BuildValue[Any]): Map[String, Any] =
       t match {
         case UnserializedValue(string) =>
           Map("stringValue" -> string)
         case SerializableBuildValue(value, serializer, mf) =>
           Map("stringValue" -> value,
               "manifest" -> JsonStructure(mf),
               "value" -> serializer(value))
       }
     def unapply(map: Map[String, Any]): Option[BuildValue[Any]] = {
       map.get("stringValue").flatMap { stringValue =>
         // TODO - Check for additional deserializers...
         val fullOpt: Option[BuildValue[Any]] = 
           for {
             rawValue <- map.get("value")
             rawMf <- map.get("manifest")
             mf <- JsonStructure.unapply[TypeInfo](rawMf.asInstanceOf[Map[String,Any]])
             result <- deserialize(rawValue.asInstanceOf[Map[String,Any]], mf)
           } yield result
         fullOpt orElse Some(UnserializedValue(stringValue.toString))
       }
     }
  }
  implicit def MyStructure[T]: protocol.RawStructure[BuildValue[T]] = 
    MyRawStructure.asInstanceOf[protocol.RawStructure[BuildValue[T]]]
  
  
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
object TaskSuccess {
  implicit def MyParamertizer[T] = 
    TaskResult.MyParamertizer[T].asInstanceOf[RawStructure[TaskSuccess[T]]]
}
/** This represents that there was an error running a task, and returns the error message. */
case class TaskFailure[T](message: String) extends TaskResult[T] {
  override def isSuccess = false
}
object TaskFailure {
  implicit def MyParamertizer[T] = 
    TaskResult.MyParamertizer[T].asInstanceOf[RawStructure[TaskFailure[T]]]
}

object TaskResult {
  implicit def MyParamertizer[T](implicit p: RawStructure[BuildValue[T]]): RawStructure[TaskResult[T]] =
    new RawStructure[TaskResult[T]] {
      def apply(t: TaskResult[T]): Map[String,Any] = 
        t match {
          case TaskFailure(msg) => Map("success" -> false, "message" -> msg)
          case TaskSuccess(value) => Map("success" -> true, "value" -> p.apply(value))
        }
      def unapply(m: Map[String, Any]): Option[TaskResult[T]] = {
        m get "success" match {
          case Some(true) => 
            for {
              rawValue <- m get "value"
              value <- p.unapply(rawValue.asInstanceOf[Map[String, Any]])
            } yield TaskSuccess(value)
          case Some(false) =>
            m.get("message").map(m => TaskFailure(m.toString))
          case _ => None
        }
      }
    }
}