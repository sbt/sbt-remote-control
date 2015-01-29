package sbt.serialization
package pickler

import scala.pickling.FastTypeTag

object TypeExpressionPicklers {
  private val typeExpressionCanToString: CanToString[TypeExpression] = CanToString(
    _.toString, {
      s: String => TypeExpression.parse(s)._1
    })
}
/** Provides a layer of pickler cake for type expressoins. */
trait TypeExpressionPicklers extends JavaExtraPicklers {
  implicit val typeExpressionPickler: SPickler[TypeExpression] with Unpickler[TypeExpression] =
    canToStringPickler[TypeExpression](FastTypeTag[TypeExpression], TypeExpressionPicklers.typeExpressionCanToString)
}