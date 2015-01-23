package sbt.serialization

import sbt.serialization.ScalaShims.ManifestFactory
import scala.pickling.FastTypeTag

object ManifestUtil {
  def isApproxIterable(tag: FastTypeTag[_], cl: ClassLoader = ManifestUtil.getClass.getClassLoader): Boolean =
    tag match {
      case x if x.key startsWith "scala.Array[" => true
      case x if x.key startsWith "scala.Option[" => true
      case x if x.key startsWith "scala.collection.immutable.Nil.type" => true
      case x if x.key startsWith "scala.collection.immutable.Vector[" => true
      case x if x.key startsWith "scala.collection.immutable.$colon$colon[" => true
      case x if x.key startsWith "scala.collection.immutable.List[" => true
      case x if x.key startsWith "scala.collection.Seq[" => true
      case x if x.key startsWith "scala.collection.immutable.Seq[" => true
      case x if x.key startsWith "scala.collection.mutable.ArrayBuffer[" => true
      case x =>
        val mitr = implicitly[Manifest[Iterable[Any]]]
        toManifest(tag, cl) map {
          _ <:< mitr
        } getOrElse false
    }
  def isApproxSubType(lhs: FastTypeTag[_], rhs: FastTypeTag[_], cl: ClassLoader = ManifestUtil.getClass.getClassLoader): Boolean =
    (lhs, rhs) match {
      case (_, x) if x.key == "scala.Any" => true
      case _ =>
        (toManifest(lhs, cl), toManifest(rhs, cl)) match {
          case (Some(lhsm), Some(rhsm)) => lhsm <:< rhsm
          case _ => false
        }
    }
  def toManifest(tag: FastTypeTag[_], cl: ClassLoader = ManifestUtil.getClass.getClassLoader): Option[Manifest[_]] =
    toManifest(tag.key, cl)

  def toManifest(key: String, cl: ClassLoader): Option[Manifest[_]] = {
    val typeExpression = TypeExpression.parse(key)._1
    toManifest(typeExpression, cl)
  }

  def toManifest(typeExpression: TypeExpression, cl: ClassLoader): Option[Manifest[_]] = {
    val args = typeExpression.typeArgs map { toManifest(_, cl) }
    if (args forall { _.isDefined }) {
      val realArgs = args.flatten
      typeExpression.typeName match {
        case "scala.Unit" => Some(ManifestFactory.Unit)
        case default =>
          try {
            val ourClass = cl.loadClass(default)
            val mf =
              if (realArgs.isEmpty) ManifestFactory.classType(ourClass)
              else ManifestFactory.classType(ourClass, realArgs.head, realArgs.tail: _*)
            Some(mf.asInstanceOf[Manifest[_]])
          } catch {
            case _: ClassNotFoundException => None
          }
      }
    } else None
  }
}
