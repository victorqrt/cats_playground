package com.victorqrt.playground

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait MyMonoid[A] extends Semigroup[A] {
  def empty: A
}

object MyMonoid {

  def apply[A](implicit mon: MyMonoid[A]) = mon

  implicit class MyMonoidOps[A](a: A) {

    def combine(x: A, y: A)(implicit ma: MyMonoid[A]): A =
      ma.combine(x, y)

    def empty(implicit ma: MyMonoid[A]) = ma.empty
  }

  /*
   * Four MyMonoids for Boolean:
   *   - && as a composition law and true as a neutral element
   *   - ||                          false
   *   - XOR                         false
   *   - XNOR                        true
   */

  implicit object BooleanMyMonoid_And extends MyMonoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = x && y
    def empty = true
  }

  implicit object BooleanMyMonoid_Or extends MyMonoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = x || y
    def empty = false
  }

  implicit object BooleanMyMonoid_Xor extends MyMonoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = (x && !y) || (!x && y)
    def empty = false
  }

  implicit object BooleanMyMonoid_Xnor extends MyMonoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = (x || !y) && (!x || y)
    def empty = true
  }
}
