package com.victorqrt.playground

import cats._
import cats.Semigroup
import cats.syntax.list._
import cats.syntax.either._
import cats.syntax.semigroup._

sealed trait Check[E, A] {

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
    this match {
      case Pure(f) => f(a)

      case And(left, right) => (left(a), right(a)) match {

        // Both failing checks (Either is left-biased)
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft

        // If any fails, fail
        case (Left(e), Right(a)) => e.asLeft
        case (Right(a), Left(e)) => e.asLeft

        case (Right(a1), Right(a2)) => a.asRight
      }
    }

  def and(that: Check[E, A]): Check[E, A] =
    And(this, that)

}

final case class Pure[E, A](f: A => Either[E, A]) extends Check[E, A]

final case class And[E, A](
  left: Check[E, A],
  right: Check[E, A]
) extends Check[E, A]
