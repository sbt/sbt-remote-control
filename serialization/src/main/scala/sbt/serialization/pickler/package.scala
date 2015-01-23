package sbt.serialization

package object pickler {
  type PrimitivePicklers = scala.pickling.pickler.PrimitivePicklers
  type PrimitiveArrayPicklers = scala.pickling.pickler.PrimitiveArrayPicklers
  type RefPicklers = scala.pickling.pickler.RefPicklers
}
