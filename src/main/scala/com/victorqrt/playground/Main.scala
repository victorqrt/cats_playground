package com.victorqrt.playground

import cats.effect._
import cats.data.EitherT
import javafx.stage.Stage
import java.io.PrintStream
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.application.Application

import com.victorqrt.playground.Gui._

object Main extends IOApp {

  type F[A] = EitherT[IO, Throwable, A]
  val F = implicitly[ConcurrentEffect[F]]

  def run(args: List[String]) =
    F.toIO {
      EitherT.right(
        IO(
          Application.launch(classOf[Playground], args: _*)
        ).map(_ => ExitCode.Success)
      )
    }
}

class Playground extends Application {

  override def start(primaryStage: Stage): Unit = {

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
    primaryStage.show

    new Thread {
      override def run: Unit = { Tests.run }
    }.start
  }
}
