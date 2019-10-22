package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated._

object DataValidationTests {

  type Errors = NonEmptyList[String]
  type StringOrErrorsPred = Predicate[Errors, String]

  def error(s: String): Errors = NonEmptyList(s, Nil)

  /*
   * Some Predicate utilities
   */

  def longerThan(n: Int): StringOrErrorsPred =
    Predicate.lift(
      error(s"Must be longer than $n chars"),
      _.size > n
    )

  def alphanumeric: StringOrErrorsPred =
    Predicate.lift(
      error("Must only contain alphanumeric chars"),
      _ forall (_.isLetterOrDigit)
    )

  def contains(c: Char): StringOrErrorsPred =
    Predicate.lift(
      error(s"Must contain $c"),
      _ contains c
    )

  /*
   * Tests
   */

  def run {

    val usernameCheck = Check(
      longerThan(3) and alphanumeric
    )

    assert(
         usernameCheck("foobar") == Valid("foobar")
      && usernameCheck("ba") == Invalid(NonEmptyList.one("Must be longer than 3 chars"))
      && usernameCheck("ba$") == Invalid(NonEmptyList.of(
           "Must be longer than 3 chars", "Must only contain alphanumeric chars"
         ))
    )

    /*

    val splitEmail: Check[Errors, String, (String, String)] =
    Check(
        Predicate {
          case s: String => s.split('@') match {
            case Array(name, domain) => NonEmptyList.of((name, domain)).valid
            case _                   => "Must contain one @".invalidNel[(String, String)]
          }
        }
      )

    val joinEmail: Check[Errors, (String, String), String] =
    Check {
        case (l, r) =>
          (longerThan(0), longerThan(3) and contains('.')).mapN(_ + "@" + _)
      }

    val checkEmail: Check[Errors, String, String] =
      splitEmail andThen joinEmail

    */
  }
}
