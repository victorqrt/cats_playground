package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.Validated
import cats.data.Validated._
import cats.Semigroup

/*
 * Predicate here builds an ADT which does not support
 * transformation of its input.
 *
 * As stated in the book, the useful thing about this data
 * structure is the following "identity law":
 *
 * Ɐ (A, E), Ɐ p ∈ Predicate[E, A], Ɐ (a1, a2) ∈ A²,
 * p(a1) == Success(a2) => a1 == a2
 */

sealed trait Predicate[E, A] {
  import Predicate._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(f) => f(a)

      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)

      case Or(left, right) => {

        left(a) match {

          case Valid(_a) => Valid(a)

          case Invalid(e) =>
            right(a) match {
              case Valid(_a) => Valid(a)
              case Invalid(_e) => Invalid(e |+| _e)
            }
        }
      }
    }

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)
}

object Predicate {

  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  final case class And[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]
  ) extends Predicate[E, A]

  final case class Or[E, A](
    left: Predicate[E, A],
    right: Predicate[E, A]
  ) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
    Pure(f)

  /*
   * Build a Predicate from a bool-valued function
   * and an error type and value.
   */

  def lift[E, A](e: E, f: A => Boolean): Predicate[E, A] =
    Pure(a => if (f(a)) a.valid else e.invalid)
}
