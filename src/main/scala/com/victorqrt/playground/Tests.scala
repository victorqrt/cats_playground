package com.victorqrt.playground

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object Tests {

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  def run {
    
    assert(add(List(1, 2, 3)) == 6)
    assert(add(List(Some(1), Some(2), None)) == Some(3))
  }
}
