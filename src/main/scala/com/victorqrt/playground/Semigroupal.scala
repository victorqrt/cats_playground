package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.Validated

case class User(name: String, age: Int)

object SemigroupalExercise {

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <-y
    } yield (a, b)


  /*
   * Some useful aliases
   */

  type Form = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailAcc[A] = Validated[List[String], A] 

  /*
   * Now the basic error-aware functions
   * Those are fail-fast
   */

  def getValue(key: String)(form: Form): FailFast[String] =
    form.get(key).toRight(List(s"$key field not specified"))

  def getName(form: Form) = getValue("name")(form)
  /* ^
   * Or: val getName = getValue("name") _
   */
  
  val getAge = getValue("age") _

  def parseInt(data: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](data.toInt)
      .leftMap(_ => List("must be an integer"))

  def nonBlank(data: String): FailFast[String] =
    Right(data).ensure(List("Unexpected empty string"))(_.nonEmpty)

  def nonNegative(data: Int): FailFast[Int] =
    Right(data).ensure(List("Unexpected negative integer"))(_ >= 0)

  def readName(form: Form): FailFast[String] =
    getName(form).flatMap(nonBlank)

  def readAge(form: Form): FailFast[Int] =
    getAge(form)
      .flatMap(nonBlank)
      .flatMap(parseInt)
      .flatMap(nonNegative)

  /*
   * We then combine those fail-fast error handlers
   * into an accumulating one
   */

  def readUser(form: Form): FailAcc[User] = (
    readName(form).toValidated,
    readAge(form).toValidated
  ).mapN(User.apply)
}
