package com.victorqrt.playground

import cats._
import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global

import com.victorqrt.playground.MyMonad._
import com.victorqrt.playground.MyFunctor._
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

      assert(kitty1.show == "Garfield is a 7 years old orange kitty")
      
      assert(kitty1 =!= kitty2)
      
      /* 
       * Here we would need to retype as we only have
       * an implicit instance for the superclass:
       * (Some(1): Option[Int]) =!= (None: Option[Int])
       */
      
      assert(Option(1) =!= Option.empty[Int])

      // Using the AND typeclass instance (see Monoids.scala)
      assert(true.empty)

      val n = 42
      assert(add(1 to n toList) == n * (n + 1) / 2)
      assert(add(List(Some(1), Some(2), None)) == Some(3))

      assert((Order(3.14, 2.0) |+| Order(2.86, 4.0)) ==  Order(6.0, 6.0))

      assert(t.map(2 * _) == Branch(Leaf(2), Leaf(4)))
      
      import com.victorqrt.playground.EvalExercise.foldRightEval
      val m = 50000
      assert(
        foldRightEval((1 to m).toList, 0: BigInt)(_ + _).value == BigInt(m) * (m + 1) / 2
      )

      import com.victorqrt.playground.WriterExercise._

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
    }

    Try(go) match {
      case Success(_) => println("All tests passed !")
      case Failure(f) => println("Error: " + f.getMessage + "\n" + f.getStackTrace.mkString("\n"))
    }
  }
}
