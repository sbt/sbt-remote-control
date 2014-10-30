package sbt.pickling

import collection.immutable.::

trait Pointed[F[_]] {
  def pointed[A: ClassManifest](a: A): F[A]
}

object Pointed {
  implicit def arrayPointed: Pointed[Array] = new Pointed[Array] {
    def pointed[A: ClassManifest](a: A): Array[A] = Array(a)
  }
  implicit def listPointed: Pointed[::] = new Pointed[::] {
    def pointed[A: ClassManifest](a: A): ::[A] = ::(a, Nil)
  }
  implicit def vectorPointed: Pointed[Vector] = new Pointed[Vector] {
    def pointed[A: ClassManifest](a: A): Vector[A] = Vector(a)
  }
}
