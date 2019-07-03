package com.victorqrt.playground

import cats._
import cats.implicits._
import java.io.PrintStream
import javafx.stage.Stage
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.application.Application

import com.victorqrt.playground.Gui._
import com.victorqrt.playground.Monoid._

object Main extends App {
  Application.launch(classOf[Playground], args: _*)

  def main {

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

    val kitty1 = Kitty("Garfield", 7, "orange")
    println(kitty1.show)

    /*
     * Eq provides methods for typesafe comparison
     */

    val optEq = Eq.apply[Option[Int]]
    
    /* Here we would need to retype as we only have
     * an implicit instance for the superclass:
     * (Some(1): Option[Int]) =!= (None: Option[Int])
     */
    assert(Option(1) =!= Option.empty[Int])

    implicit val catEq: Eq[Kitty] = Eq.instance[Kitty] {
      (k1, k2) => (
        k1.name === k2.name && k1.age === k2.age && k1.color === k2.color
      )
    }

    val kitty2 = Kitty("Brice", 9, "blue")
    assert(kitty1 =!= kitty2)

    assert(Option(kitty1) =!= Option.empty[Kitty])
  }
}

class Playground extends Application {

  override def start(primaryStage: Stage) {
    
    /*
     * JavaFX setup
     */

    val loader = new FXMLLoader(getClass.getClassLoader.getResource("gui.fxml"))
    val root = loader.load[Parent]
    primaryStage.setTitle("Playground")
    primaryStage.setScene(new Scene(root))

    val guiController = loader.getController[Gui]
    var ps = new PrintStream(
      new Console(guiController.txtArea),
      true
    )
    
    System.setOut(ps);
    System.setErr(ps);
    
    primaryStage.sizeToScene();
    primaryStage.setResizable(false);
    primaryStage.show

    Main.main
  }
}
