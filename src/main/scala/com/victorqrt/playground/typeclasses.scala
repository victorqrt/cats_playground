object TypeClasses {

  case class Person(name: String, email: String)

  trait Eq[A] { def equals(a1: A, a2: A): Boolean }

  /*object EmailEq extends Eq[Person] {
    def equals(a1: Person, a2: Person): Boolean =
      a1.email == a2.email
  }*/

  implicit object NameEmailEq extends Eq[Person] {
    def equals(a1: Person, a2: Person): Boolean =
      a1.name == a2.name
  }

  /*object Eq {
    def apply[A](a1: A, a2: A)(implicit e: Eq[A]): Boolean =
      e.equals(a1, a2)
  }

  println(Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com")))
  */

  implicit class EqClass[A](a: A) {
    def =?=(_a: A)(implicit e: Eq[A]): Boolean =
      e.equals(a, _a)
  }

  assert(Person("Noel", "noel2222@example.com") =?= Person("Noel", "noel@example.com"))
}
