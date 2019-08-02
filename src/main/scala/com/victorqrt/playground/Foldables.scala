package com.victorqrt.playground

import cats._
import cats.implicits._

object FoldableExercise {

  def mapWithFold[A, B](as: List[A])(f: A => B): List[B] =
    as.foldRight(Nil: List[B])(f(_) :: _)

  def flatMapWithFold[A, B](as: List[A])(f: A => List[B]): List[B] =
    as.foldRight(Nil: List[B])(f(_) ::: _)

  def filterWithFold[A](as: List[A])(f: A => Boolean): List[A] =
    as.foldRight(Nil: List[A]) (
      (a, acc) => if (f(a)) a :: acc else acc
    )

  def sumWithFold[A](as: List[A])(implicit mon: Monoid[A]): A =
    as.foldRight(mon.empty)(mon.combine)
}
