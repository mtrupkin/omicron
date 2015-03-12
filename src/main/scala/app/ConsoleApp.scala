package me.mtrupkin.game.app

import javafx.application.Application
import javafx.scene.image.Image
import javafx.stage.Stage

import me.mtrupkin.controller.Controller

import scala.collection.JavaConversions._

class ConsoleApp extends Application {

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Omicron")

    val rs = List("/icons/icon-16.png", "/icons/icon-32.png", "/icons/icon-64.png")
    primaryStage.getIcons.addAll(rs.map(new Image(_)))

    object Controller extends Controller {
      lazy val css = "/views/Console.css"
      lazy val initialState: ControllerState = new IntroController
      lazy val stage = primaryStage
    }

    Controller.stage.show()
  }

  def icons(rs: Seq[String]): Seq[Image] = {
    rs.map(r => new Image(r))
  }
}

object ConsoleApp extends App {
	Application.launch(classOf[ConsoleApp], args: _*)
}