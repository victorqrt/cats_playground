object Main extends App {

  import JSON._
  
  implicit object IntJsInstance extends JsWriter[Int] {
    def write(i: Int) = JsInt(i)
  }

  implicit object DoubleJsInstance extends JsWriter[Double] {
    def write(d: Double) = JsDouble(d)
  }

  implicit object StringJsInstance extends JsWriter[String] {
    def write(s: String) = JsString(s)
  }

  /*val obj = JsObject(
    Map(
      "foo" -> JsString("bar"),
      "baz" -> JsString("waldo"),
      "map" -> JsObject(
        Map(
          "int" -> JsInt(788),
          "doubles" -> JsArray(
            List(JsDouble(3.14), JsDouble(2.16))
          )
        )
      )
    )
  )*/

  val obj = JsObject(
    Map(
      "foo" -> "bar".toJson,
      "baz" -> "waldo".toJson,
      "map" -> JsObject(
        Map(
          "ints" -> JsArray(
            (0 until 10).toList.map(_.toJson),
          ),
          "doubles" -> JsArray(
            List(3.14.toJson, 2.16.toJson)
          )
        )
      )
    )
  )

  println(obj)
}

object JSON {

  /*
   * JSON ADT
   */
  
  sealed trait JsValue { override def toString: String }
  
  final case class JsObject(values: Map[String, JsValue]) extends JsValue {
    override def toString = values
      .map { case(name, value) => "\"" + name + "\":" + value.toString }
      .mkString("{", ",", "}")
  }
  
  final case class JsArray(values: List[JsValue]) extends JsValue {
    override def toString = values.mkString("[", ",", "]")
  }
  
  final case class JsString(value: String) extends JsValue {
    override def toString = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
  }
  
  sealed trait JsNumber extends JsValue
  
  final case class JsInt(value: Int) extends JsNumber {
    override def toString = value.toString
  }
  
  final case class JsDouble(value: Double) extends JsNumber {
    override def toString = value.toString
  }
  
  /*
   * Type class
   */
  
  trait JsWriter[A] { def write(a: A): JsValue }
  
  implicit class JsWriterClass[A](a: A) {
    def toJson(implicit jw: JsWriter[A]): JsValue =
      jw write a
  }
}
