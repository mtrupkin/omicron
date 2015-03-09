package me.mtrupkin.controller


import java.io.{FileWriter, Writer}
import java.nio.file.{StandardOpenOption, OpenOption, Paths, Files}
import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.Pane

import me.mtrupkin.console.{Modifiers, ConsoleKey}
import me.mtrupkin.game.model.TileMap._
import me.mtrupkin.game.model._
import rexpaint.RexPaintImage
import me.mtrupkin.core.{Point, Size}

import scalafx.scene.{control => sfxc}
import scalafx.scene.{layout => sfxl}
import scalafx.scene.{input => sfxi}
import scalafx.Includes._


/**
 * Created by mtrupkin on 12/15/2014.
 */


trait Intro { self: Controller =>

  class IntroController extends ControllerState {
    val name = "Intro"

    @FXML var pane: Pane = _
    @FXML var continueGameButton: Button = _

    def initialize(): Unit = {
      continueGameButton.setDisable(!World.exists())
      new sfxl.Pane(pane) {
        onKeyPressed = (e: sfxi.KeyEvent) => handleKeyPressed(e)
      }
    }

    def handleContinueGame(event: ActionEvent) = {
      val world = World.read()

      changeState(new GameController(new CombatTracker(world)))
    }

    def handleNewGame(event: ActionEvent) = {
      val levelName = "layers-1"

      // create world

      val is = getClass.getResourceAsStream(s"/levels/$levelName.xp")
      val image = RexPaintImage.read(levelName, is)
      val tileMap = TileMap.load(levelName, image.size, image.layers.head.matrix)

      val (player, agents) = Entity.toEntities(image.layers(1).matrix)
      val world = new World(agents, player, tileMap)

      World.write(world)


      changeState(new GameController(new CombatTracker(world)))
    }

    def handleOptions(event: ActionEvent) = {
      flipState(new HelpController)
    }

    def handleExit(event: ActionEvent) = stage.close()

    def handleKeyPressed(event: sfxi.KeyEvent): Unit = {
      import me.mtrupkin.console.Key._
      val key = keyCodeToConsoleKey(event)
      key match {
        case ConsoleKey(X, Modifiers.Control) => exit()
        case ConsoleKey(k, _) => k match {
          case Esc => flipState(new HelpController)
          case _ =>
        }
        case _ =>
      }
    }
  }
}
