package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.Validated
import cats.data.Validated._
import cats.Semigroup

/*
 * We build Check[E, A, B], allowing us
 * to build Bs from a Predicate's A input.
 */

sealed trait Check[E, A, B] {
  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    MapDT[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
    FlatMapDT[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThenDT(this, that)
}

object Check {

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] =
    PureCheck(p)

  final case class PureCheck[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      p(a)
  }

  final case class MapDT[E, A, B, C](c: Check[E, A, B], f: B => C)
    extends Check[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      c(a) map f
  }

  final case class FlatMapDT[E, A, B, C](c: Check[E, A, B], f: B => Check[E, A, C])
    extends Check[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      c(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThenDT[E, A, B, C](c1: Check[E, A, B], c2: Check[E, B, C])
    extends Check[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      c1(a).withEither(_.flatMap(b => c2(b).toEither))
  }
}
