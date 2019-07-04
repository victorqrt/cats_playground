package com.victorqrt.playground

import java.io.PrintStream
import javafx.stage.Stage
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.application.Application

import com.victorqrt.playground.Gui._

object Main extends App {
  Application.launch(classOf[Playground], args: _*)
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
    primaryStage.show

    Tests.run
  }
}
