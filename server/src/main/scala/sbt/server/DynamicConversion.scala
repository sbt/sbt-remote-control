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
    val ret = registered.get(mf).map { conversion =>
      val result = conversion.asInstanceOf[RegisteredConversion[F, _]].convert(value)
      (result, conversion.toManifest)
    }
    ret orElse defaultConversionByManifest(value, mf)
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

  private def defaultConversionByManifest[F](value: F, mf: Manifest[F]): Option[(_, Manifest[_])] = {
    mf.runtimeClass match {
      case Classes.SeqSubClass() =>
        defaultConversionForSeq(value.asInstanceOf[Seq[_]], mf.typeArguments.head)
      case _ =>
        None
    }
  }

  private def defaultConversionForSeq[T](xs: Seq[_], mf: Manifest[T]): Option[(Seq[_], Manifest[_])] = {
    mf.runtimeClass match {
      case Conversions.original.Attributed =>
        defaultConversionForSeqAtt(xs.asInstanceOf[Seq[sbt.Attributed[_]]], mf.typeArguments.head.runtimeClass)
      case _ =>
        None
    }
  }

  private def defaultConversionForSeqAtt(atts: Seq[sbt.Attributed[_]], klass: Class[_]): Option[(Seq[protocol.Attributed[_]], Manifest[_])] = {
    klass match {
      case Classes.FileClass =>
        val mapped = atts.asInstanceOf[Seq[sbt.Attributed[File]]] map SbtToProtocolUtils.attributedToProtocol
        Some((mapped, manifest[Seq[protocol.Attributed[File]]]))
      case _ =>
        None
    }
  }
}
