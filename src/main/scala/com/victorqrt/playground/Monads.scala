package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.{Reader, State, Writer}
import scala.annotation.tailrec

import com.victorqrt.playground.MyFunctor._
import com.victorqrt.playground.MyFunctor.Tree._

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

object ReaderExercise {

  import com.victorqrt.playground.Tests.Kitty

  val kittyName: Reader[Kitty, String] = Reader(_.name)

  val greetKitty: Reader[Kitty, String] = kittyName map (name => s"Hello $name")

  val feedKitty: Reader[Kitty, String] = Reader(kitty => s"Eat something, ${kitty.name}")

  val greetAndFeed: Reader[Kitty, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."

  final case class Db(
    users: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(uid: Int): DbReader[Option[String]] =
    Reader(_.users get uid)

  def checkPassword(user: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(user) contains password)

  /*
  def checkLogin(uid: Int, password: String): DbReader[Boolean] =
    for {
      user <- findUsername(uid)
      matches <- user.map(checkPassword(_, password)) getOrElse false.pure[DbReader]
    } yield matches
  */
}

object PostOrderCalc {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case n => operand(n.toInt)
    }

  def evalAll(syms: List[String]): CalcState[Int] =
    syms.foldLeft(evalOne("0")) {
      (a, b) => a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  def operand(n: Int): CalcState[Int] =
    State[List[Int], Int] {
      s => (n :: s, n)
    }

  def operator(f: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {

      case a :: b :: t => {
        val ans = f(a, b)
        (ans :: t, ans)
      }

      case _ => sys.error("Fail")
    }
}

object TreeExercise {

  implicit object TreeMonad extends Monad[Tree] {

    def pure[A](a: A) = Leaf(a)

    def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
      tree.fold(f)(Branch(_, _))

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: closed)

          case Leaf(Left(value)) :: next =>
            loop(f(value) :: next, closed)

          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: closed)

          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) {
              (acc, maybeTree) => {
                maybeTree.map(_ :: acc).getOrElse {
                  val left :: right :: tail = acc
                  branch(left, right) :: tail
                }
              }
            }
        }

      loop(List(f(a)), Nil).head
    }
  }
}
