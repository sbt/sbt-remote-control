package sbt
package server

import sbt.protocol._

/**
 * An interface representing a mechanism to convert types
 * using their runtime manifest.
 *
 *  This suffers from all the same limitations as scala.Manifest for
 *  handling types.
 *
 *  A DynamicConversion is immutable.
 */
sealed trait DynamicConversion {
  def convertWithRuntimeClass[F](value: F): Option[(_, Manifest[_])]
  def convert[F](value: F)(implicit mf: Manifest[F]): Option[(_, Manifest[_])]
  /** Add a conversion, returning the new modified DynamicConversion. */
  def register[F, T](convert: F => T)(implicit fromMf: Manifest[F], toMf: Manifest[T]): DynamicConversion
  def ++(other: DynamicConversion): DynamicConversion
}

object DynamicConversion {
  val empty: DynamicConversion = ConcreteDynamicConversion(Map.empty, Map.empty)
}

private final case class RegisteredConversion[F, T](fromManifest: Manifest[F], toManifest: Manifest[T], convert: F => T)

private final case class ConcreteDynamicConversion(registered: Map[Manifest[_], RegisteredConversion[_, _]],
  byClass: Map[Class[_], RegisteredConversion[_, _]]) extends DynamicConversion {
  override def convertWithRuntimeClass[F](value: F): Option[(_, Manifest[_])] = {
    byClass.get(value.getClass).map { conversion =>
      val result = conversion.asInstanceOf[RegisteredConversion[F, _]].convert(value)
      (result, conversion.toManifest)
    }
  }

  override def convert[F](value: F)(implicit mf: Manifest[F]): Option[(_, Manifest[_])] = {
    registered.get(mf).map { conversion =>
      val result = conversion.asInstanceOf[RegisteredConversion[F, _]].convert(value)
      (result, conversion.toManifest)
    }
  }

  override def register[F, T](convert: F => T)(implicit fromMf: Manifest[F], toMf: Manifest[T]): DynamicConversion = {
    val conversion = RegisteredConversion(fromMf, toMf, convert)
    copy(registered = registered + (fromMf -> conversion), byClass = byClass + (fromMf.runtimeClass -> conversion))
  }

  override def ++(other: DynamicConversion): DynamicConversion = {
    other match {
      case c: ConcreteDynamicConversion => copy(registered = (registered ++ c.registered))
    }
  }
}
