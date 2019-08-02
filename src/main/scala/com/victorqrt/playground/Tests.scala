package com.victorqrt.playground

import cats._
import cats.implicits._
import cats.data.EitherT
import scala.concurrent._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import cats.data.Validated.{Valid, Invalid}
import scala.concurrent.ExecutionContext.Implicits.global

import com.victorqrt.playground.MyMonad._
import com.victorqrt.playground.MyFunctor._
import com.victorqrt.playground.TreeExercise._
import com.victorqrt.playground.PostOrderCalc._
import com.victorqrt.playground.WriterExercise._
import com.victorqrt.playground.ReaderExercise._
import com.victorqrt.playground.MyFunctor.Tree._
import com.victorqrt.playground.FoldableExercise._
import com.victorqrt.playground.MonadTransformers._
import com.victorqrt.playground.SemigroupalExercise._
import com.victorqrt.playground.EvalExercise.foldRightEval
import com.victorqrt.playground.MyMonoid.{BooleanMyMonoid_And, MyMonoidOps}

object Tests {

  /*
   * The Show typeclass is encoding some toString-like behaviour
   */

  val stringShow = Show.apply[String]
  val intShow = Show.apply[Int]

  final case class Kitty(name: String, age: Int, color: String)

  // Show.show[A](f: A => String): Show[A]
  implicit val kittyShow: Show[Kitty] = Show.show {
    k => s"${k.name.show} is a ${k.age.show} years old ${k.color.show} kitty"
  }

  /*
   * Eq provides methods for typesafe comparison
   */

  val optEq = Eq.apply[Option[Int]]

  implicit val catEq: Eq[Kitty] = Eq.instance[Kitty] {
    (k1, k2) => (
      k1.name === k2.name && k1.age === k2.age && k1.color === k2.color
    )
  }

  val kitty1 = Kitty("Garfield", 7, "orange")
  val kitty2 = Kitty("Brice", 9, "blue")

  /*
   * 2.5.4
   */

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  final case class Order(totalCost: Double, qty: Double)

  implicit object OrderMonoid extends Monoid[Order] {

    def combine(o1: Order, o2: Order): Order =
      Order(
        o1.totalCost + o2.totalCost,
        o1.qty + o2.qty
      )

    def empty = Order(0, 0)
  }

  /*
   * 3.5.4
   */

  val t: Tree[Int] = Branch(Leaf(1), Leaf(2))

  /*
   * Ran by the main app
   */

  def run {

    def go {

      println("Running  tests...")

      assert(
           kitty1.show == "Garfield is a 7 years old orange kitty"
        && kitty1 =!= kitty2
      )

      /*
       * Here we would need to retype as we only have
       * an implicit instance for the superclass:
       * (Some(1): Option[Int]) =!= (None: Option[Int])
       */

      assert(Option(1) =!= Option.empty[Int])

      // Using the AND typeclass instance (see Monoids.scala)
      assert(true.empty)

      val n = 42
      assert(
           add(1 to n toList) == n * (n + 1) / 2
        && add(List(Some(1), Some(2), None)) == Some(3)
        && (Order(3.14, 2.0) |+| Order(2.86, 4.0)) ==  Order(6.0, 6.0)
        && t.map(2 * _) == Branch(Leaf(2), Leaf(4))
      )

      val m = 50000
      assert(
        foldRightEval((1 to m).toList, 0: BigInt)(_ + _).value == BigInt(m) * (m + 1) / 2
      )

      val Vector((log1, ans1), (log2, ans2)) =
        Await.result(
          Future.sequence(
            Vector(
              Future(factorial(3).run),
              Future(factorial(4).run)
            )
          ),
          5.seconds
        )

      assert(
           log1.mkString(", ") == "fact 0: 1, fact 1: 1, fact 2: 2, fact 3: 6"
        && ans1 == 6
        && log2.mkString(", ") == "fact 0: 1, fact 1: 1, fact 2: 2, fact 3: 6, fact 4: 24"
        && ans2 == 24
      )

      assert(
           kittyName.run(kitty1) == "Garfield"
        && greetKitty.run(kitty1) == "Hello Garfield"
        && greetAndFeed.run(kitty1) == "Hello Garfield. Eat something, Garfield."
      )

      val myDB = Db(
        Map(0 -> "root", 1 -> "user"),
        Map("root" -> "root", "user" -> "pass")
      )

      assert(
           findUsername(0).run(myDB) == Some("root")
        && findUsername(42).run(myDB) == None
        && checkPassword("user", "pass").run(myDB)
        && !checkPassword("root", "pass").run(myDB)
        && !checkPassword("john", "pass").run(myDB)
      )

      assert(
           evalOne("42").runA(Nil).value == 42
        && evalAll(List("6", "6", "+", "5", "*")).runA(Nil).value == 60
        && evalInput("1 2 + 3 4 + *") == 21
      )

      val treeTest =
        for {
          t1 <- branch(leaf(2), t)
          newT <- leaf(2 * t1)
        } yield newT

      assert(
        treeTest == Branch(Leaf(4), Branch(Leaf(2), Leaf(4)))
      )

      assert(
           Await.result(getPowerLevel("Jazz").value, 1.second) == Right(6)
        && Await.result(getPowerLevel("Goldorak").value, 1.second) == Left("Goldorak unreachable")
        && Await.result(canSpecialMove("Jazz", "Hot Rod").value, 1.second)  == Right(true)
        && Await.result(canSpecialMove("Jazz", "Bumblebee").value, 1.second)  == Right(false)
        && Await.result(canSpecialMove("Jazz", "Goldorak").value, 1.second) == Left("Goldorak unreachable")
        && tacticalReport("Jazz", "Hot Rod") == "Yes"
      )

      assert(
           getName(Map("name" -> "Guthrie Govan")) == Right("Guthrie Govan")
        && getName(Map()) == Left(List("name field not specified"))
        && parseInt("484") == Right(484)
        && parseInt("foo") == Left(List("must be an integer"))
        && readUser(Map("age" -> "64", "name" -> "Bernard Tapie")) == Valid(
             User("Bernard Tapie", 64)
           )
        && readUser(Map("age" -> "-64", "name" -> "")) == Invalid(List(
            "Unexpected empty string",
            "Unexpected negative integer"
           ))
      )

      val lst: List[Int] = (1 to 100 toList)
      assert(
           lst.foldLeft(Nil: List[Int])((a, i) => i :: a) == lst.reverse
        && lst.foldRight(Nil: List[Int])((i, a) => i :: a) == lst
        && mapWithFold(lst)(2 * _) == lst.map(2 * _)
        && flatMapWithFold(lst)(x => List(x)) == lst.flatMap(x => List(x))
        && filterWithFold(lst)(_ % 2 == 0) == lst.filter(_ % 2 == 0)
        && sumWithFold(lst) == lst.sum
      )
    }

    Try(go) match {
      case Success(_) => println("All tests passed !")
      case Failure(f) => println("Error: " + f.getMessage + "\n" + f.getStackTrace.mkString("\n"))
    }
  }
}
