package com.victorqrt.playground

import cats._
import cats.implicits._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MapReduce {

  /*
   * We either add (implicit m: Monoid[B]) as an
   * implicit parameter list, or, like here, a Monoid
   * context bound on the B type parameter
   */

  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    as.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
    // Or as.foldLeft(Monoid[B].empty)(_ |+| f(_))
    // Or as.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)

  def parallelFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = {

    val cores = Runtime.getRuntime.availableProcessors
    val tasks = as.grouped((as.size.toDouble / cores).ceil.toInt)

    Future
      .sequence(tasks.map(t => Future(foldMap(t)(f))))
      .map(t => t.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  def catsParallelFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = {

    val cores = Runtime.getRuntime.availableProcessors
    val taskSize = (as.size.toDouble / cores).ceil.toInt

    as.grouped(taskSize)
      .toVector
      .traverse(t => Future(t.toVector.foldMap(f)))
      .map(_.combineAll)
  }
}
