package com.victorqrt.playground

import cats.Functor

object MyFunctor {

  sealed trait Tree[+A] {

    def fold[B](f: A => B)(g: (B, B) => B): B =
      this match {
        case Leaf(value) => f(value)
        case Branch(left, right) => g(left.fold(f)(g), right.fold(f)(g))
      }
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  /*
   * The whole point is to not have .map natively on Tree instances, hence the object
   */

  object Tree {

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t.fold(x => Leaf(f(x)): Tree[B])(Branch(_, _))

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](a: A): Tree[A] = Leaf(a)
  }

  /*
   * This is commented because we introduce a Monad instance
   * for Tree later on
   *
  implicit object TreeFunctor extends Functor[Tree] {
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      Tree.map(t)(f)
  }*/
}
