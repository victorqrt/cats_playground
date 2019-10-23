package com.victorqrt.playground

import cats.kernel.CommutativeMonoid

final case class GCounter(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int): GCounter = {
    val i = counters.getOrElse(machine, 0) + amount
    GCounter(counters + (machine -> i))
  }

  def merge(that: GCounter): GCounter =
    GCounter(
      counters ++ that.counters map {
        case (k, v) =>
          k -> (v max counters.getOrElse(k, 0))
      }
    )

  def total: Int =
    counters.values.sum
}

object CrdtTypeclasses {

  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  implicit class BoundedSemiLatticeOps[A](a: A) {
    def combine(_a: A)(implicit bsa: BoundedSemiLattice[A]) =
      bsa.combine(a, _a)
  }

  implicit object IntBoundedSemiLattice extends BoundedSemiLattice[Int] {
    val empty = 0
    def combine(i1: Int, i2: Int) =
      i1 max i2
  }

  implicit def SetBoundedSemiLattice[A] =
    new BoundedSemiLattice[Set[A]] {
      val empty = Set.empty
      def combine(s1: Set[A], s2: Set[A]) =
        s1 | s2
    }
}
