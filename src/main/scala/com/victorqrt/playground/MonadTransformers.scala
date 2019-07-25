package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.EitherT
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
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

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      a1 <- getPowerLevel(ally1)
      a2 <- getPowerLevel(ally2)
    } yield a1 + a2 >= 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Left(_) => "No, inexisting ally"
      case Right(false) => "No, not enough power"
      case Right(true) => "Yes"
    }
}
