package com.victorqrt.playground

import java.io.OutputStream
import javafx.fxml.FXML
import javafx.scene.control._
import javafx.application.Platform

class Gui {

  @FXML var exit: Button = _
  @FXML var txtArea: TextArea = _

  @FXML def exitapp = System.exit(0)
}

object Gui {

  class Console(var output: TextArea) extends OutputStream {
    override def write(i: Int) {
      Platform.runLater(() => output.appendText(String.valueOf(i.asInstanceOf[Char])));
    }
  }
}
