// For testing
final case class Cat(name: String, age: Int, color: String)

// Type class
trait Printable[-A] {
  def printify(value: A): String
}

// Instances
object Printable {

  implicit class _Printable[A](a: A) {
    def printify(implicit p: Printable[A]): String =
      p.printify(a)
  }

  // Two different syntaxes for typeclass instances:
    
  // Lazy by default
  implicit object StringPrintable extends Printable[String] {
    def printify(value: String) = value
  }

  // Eager by default, overridable
  implicit val IntPrintable = new Printable[Int] {
    def printify(value: Int) = value.toString
  }

  implicit object CatPrintable extends Printable[Cat] {
    def printify(value: Cat) =
      s"${value.name.printify} is a ${value.age.printify} years old ${value.color.printify} cat"
  }
}
