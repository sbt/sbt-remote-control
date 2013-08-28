package com.typesafe.sbtrc
package controller


import protocol._

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

// TODO - Figure out how to serialize arbitrary values using Parametizable 




sealed trait TaskResult[T] {
  /** Returns whether or not a task was executed succesfully. */
  def isSuccess: Boolean
}
case class TaskSuccess[T](value: BuildValue[T]) extends TaskResult[T] {
  override def isSuccess = true
}
case class TaskFailure[T](message: String) extends TaskResult[T] {
  override def isSuccess = false
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