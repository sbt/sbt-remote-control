package sbt.serialization

/** Utilities used in PrimitivePicklers.  Note: We can remove this once we clean up scala/pickling. */
private[serialization] trait RichTypes {
  import scala.reflect.runtime.universe._

  implicit class RichType(tpe: scala.reflect.api.Universe#Type) {
    import definitions._
    def isEffectivelyPrimitive: Boolean = tpe match {
      case TypeRef(_, sym: ClassSymbol, _) if sym.isPrimitive => true
      case TypeRef(_, sym, eltpe :: Nil) if sym == ArrayClass && eltpe.typeSymbol.isClass && eltpe.typeSymbol.asClass.isPrimitive => true
      case _ => false
    }
  }
}
