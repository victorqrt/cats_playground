package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.EitherT
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MonadTransformers {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
  )

  def getPowerLevel(name: String): Response[Int] =
    powerLevels.get(name) match {
      case Some(value) => EitherT.right(Future(value))
      case None => EitherT.left(Future(s"$name unreachable"))
    }

}
