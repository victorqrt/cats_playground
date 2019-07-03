package com.victorqrt.playground

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  
  def apply[A](implicit mon: Monoid[A]) = mon

  implicit class MonoidClass[A](a: A) {

    def combine(x: A, y: A)(implicit ma: Monoid[A]): A =
      ma.combine(x, y)
    
    def empty(implicit ma: Monoid[A]) = ma.empty
  }

  /*
   * Four monoids for Boolean: 
   *   - && as a composition law and true as a neutral element
   *   - ||                          false
   *   - XOR                         false
   *   - XNOR                        true
   */

  implicit object BooleanMonoid_And extends Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = x && y
    def empty = true  
  }

  implicit object BooleanMonoid_Or extends Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = x || y
    def empty = false
  }

  implicit object BooleanMonoid_Xor extends Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = (x && !y) || (!x && y)
    def empty = false
  }

  implicit object BooleanMonoid_Xnor extends Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = (x || !y) && (!x || y)
    def empty = true
  }
}
