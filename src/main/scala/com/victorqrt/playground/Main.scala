package com.victorqrt.playground

import cats._
import cats.implicits._

object Main extends App {

  /*
   * The Show typeclass is encoding some "toString"-like behaviour
   */

  val stringShow = Show.apply[String]
  val intShow = Show.apply[Int]

  final case class Kitty(name: String, age: Int, color: String)

  implicit val kittyShow: Show[Kitty] = Show.show {
    case k => s"${k.name.show} is a ${k.age.show} years old ${k.color.show} kitty"
  }

  println(Kitty("Garfield", 12, "red").show)

  /*
   * Eq provides methods for typesafe comparison
   */

  val optEq = Eq.apply[Option[Int]]
  println(
    // We would need to retype as we only have an implicit instance for the superclass
    // (Some(1): Option[Int]) =!= (None: Option[Int])
    Option(1) =!= Option.empty[Int]
  )

}