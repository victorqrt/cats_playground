package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.Writer

trait MyMonad[F[_]] {
    
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  def map[A, B](value: F[A])(f: A => B): F[B] =
    flatMap(value)(a => pure(f(a)))
}

object MyMonad {

  def pure[A](a: A): Id[A] = a

  def flatMap[A, B](ia: Id[A])(f: A => Id[B]): Id[B] = f(ia)

  def map[A, B](ia: Id[A])(f: A => B): Id[B] = f(ia)
}

object EvalExercise {

  def foldRightEval[A, B](as: List[A], z: B)(f: (A, B) => B): Eval[B] =
    as match {
      case Nil => Eval.now(z)
      case h :: t => Eval.defer(foldRightEval(t, f(h, z))(f))
    }
}

object WriterExercise {

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorial(n - 1).map(n * _))
      _ <- Vector(s"fact $n: $ans").tell
    } yield ans
}
