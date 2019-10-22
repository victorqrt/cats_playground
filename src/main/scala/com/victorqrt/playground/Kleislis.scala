package com.victorqrt.playground

import cats._
import cats.data._
import cats.implicits._

import com.victorqrt.playground.DataValidationTests._

object Kleislis {

  type Errors       = NonEmptyList[String]
  type Result[A]    = Either[Errors, A]
  type KCheck[A, B] = Kleisli[Result, A, B]

  def kCheck[A, B](f: A => Result[B]): KCheck[A, B] =
    Kleisli(f)

  def kCheckPred[A](p: Predicate[Errors, A]): KCheck[A, A] =
    Kleisli[Result, A, A](p.run)

  def error(s: String) = NonEmptyList(s, Nil)

  def containsOnce(c: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain $c only once"),
      s => (s filter (_ == c)).size == 1
    )

  val checkUserName: KCheck[String, String] =
    kCheckPred(longerThan(3) and alphanumeric)

  val splitEmail: KCheck[String, (String, String)] =
    kCheck(
      _ split '@' match {
        case Array(user, domain) =>
          Right(user, domain)
        case _ =>
          Left(error("Must contain @ only once"))
      }
    )

  val checkLeft: KCheck[String, String] =
    kCheckPred(longerThan(0))

  val checkRight: KCheck[String, String] =
    kCheckPred(longerThan(3) and contains('.'))

  val joinEmail: KCheck[(String, String), String] =
    kCheck {
      case (l, r) =>
        (checkLeft(l), checkRight(r)) mapN (_ + "@" + _)
    }

  val checkEmail: KCheck[String, String] =
    splitEmail andThen joinEmail

  def run {
    val email = "vquerette@gmail.com"
    assert(
         checkEmail.run(email) == Right(email)
      && checkEmail.run("@" + email) == Left(error("Must contain @ only once"))
    )
  }
}