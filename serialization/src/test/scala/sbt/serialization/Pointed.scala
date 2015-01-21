package sbt.serialization.spec

trait Pointed[F[_]] {
  def pointed[A: ClassManifest](a: A): F[A]
}

object Pointed {
  implicit def arrayPointed: Pointed[Array] = new Pointed[Array] {
    def pointed[A: ClassManifest](a: A): Array[A] = Array(a)
  }
  implicit def listPointed: Pointed[List] = new Pointed[List] {
    def pointed[A: ClassManifest](a: A): List[A] = List(a)
  }
  implicit def vectorPointed: Pointed[Vector] = new Pointed[Vector] {
    def pointed[A: ClassManifest](a: A): Vector[A] = Vector(a)
  }
}
