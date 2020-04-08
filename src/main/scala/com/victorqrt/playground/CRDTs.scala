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
        case (k, v) => k -> (v max counters.getOrElse(k, 0))
      }
    )

  def total: Int =
    counters.values.sum
}

object CrdtTypeclasses {

  trait BoundedSemiLattice[A] {
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

  /*
   * GC3 instance for Map - see 11.4
   */

  implicit def MapGC3[K, V] =
    new GC3[Map, K, V] {

      def increment(counters: Map[K, V])
                   (k: K, v: V)
                   (implicit cm: CommutativeMonoid[V]): Map[K, V] = {

        val i = cm.combine(counters.getOrElse(k, cm.empty), v)
        counters + (k -> i)
      }

      def merge(c1: Map[K, V], c2: Map[K, V])
               (implicit bsl: BoundedSemiLattice[V]): Map[K, V] =
        c1 ++ c2 map {
          case (k, v) => k -> (v combine c1.getOrElse(k, bsl.empty))
        }

      def total(counters: Map[K, V])
               (implicit cm: CommutativeMonoid[V]): V =
        cm combineAll counters.values
    }
}

import CrdtTypeclasses._

/*
 * 11.3.3 - Generalizing the values
 */

final case class GC2[K, V : BoundedSemiLattice : CommutativeMonoid]
  (counters: Map[K, V]) {

  val zero = implicitly[BoundedSemiLattice[V]].empty

  def increment(machine: K, amount: V): GC2[K, V] = {
    val i = counters.getOrElse(machine, zero) combine amount
    GC2(counters + (machine -> i))
  }

  def merge(that: GC2[K, V]): GC2[K, V] =
    GC2(
      counters ++ that.counters map {
        case (k, v) => k -> (v combine counters.getOrElse(k, zero))
      }
    )

  def total: V =
    implicitly[CommutativeMonoid[V]] combineAll counters.values
}

/*
 * 11.4 - Abstracting over "container"
 */

trait GC3[F[_, _], K, V] {

  type Store = F[K, V]

  def increment(f: Store)
               (k: K, v: V)
               (implicit cm: CommutativeMonoid[V]): Store

  def merge(f1: Store, f2: Store)
           (implicit bsl: BoundedSemiLattice[V]): Store

  def total(f: Store)
           (implicit cm: CommutativeMonoid[V]): V
}

object GC3 {
  def apply[F[_, _], K, V](implicit c: GC3[F, K, V]) = c
}

object CrdtTests {

  import cats.instances.int._

  def run: Unit = {

    val gc1   = GCounter(Map("a" -> 2, "b" -> 0))
    val gc2   = GCounter(Map("a" -> 1, "b" -> 3))

    val gc2_1 = GC2[String, Int](Map("a" -> 2, "b" -> 0))
    val gc2_2 = GC2[String, Int](Map("a" -> 1, "b" -> 3))

    val gc3_1 = Map("a" -> 7, "b" -> 3)
    val gc3_2 = Map("a" -> 2, "b" -> 5)

    val gctr  = GC3[Map, String, Int]

    assert(
         (gc1 merge gc2) == GCounter(Map("a" -> 2, "b" -> 3))
      && (gc1.increment("b", 2)).total == 4
      && (gc2_1 merge gc2_2) == GC2(Map("a" -> 2, "b" -> 3))
      && (gc2_1.increment("b", 2)).total == 4
      && gctr.total(gctr.merge(gc3_1, gc3_2)) == 12
    )
  }
}
