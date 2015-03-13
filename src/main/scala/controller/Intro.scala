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
import me.mtrupkin.core.{Points, Point, Size}

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

      // create world
      val levelPosition2 = Point(1,0)
      val levelPosition3 = Point(2,0)
      val (startLevel, player) = TileMap.loadStartLevel("start")
      val (level2, agents2) = TileMap.loadLevel("tile-2")
      val (level3, agents3) = TileMap.loadLevel("tile-3")

      val levelSize = startLevel.size
      val levelMap = new LevelMap("mission-1", levelSize)
      levelMap.tileMaps(Points.Origin) = startLevel
      levelMap.tileMaps(levelPosition2) = level2
      levelMap.tileMaps(levelPosition3) = level3

      val viewPort = new ViewPort(Size(levelSize.width * 3, levelSize.height), Points.Origin, levelMap)



      val agents = levelMap.toGlobalAgent(levelPosition2, agents2) ++ levelMap.toGlobalAgent(levelPosition3, agents3)

      val world = new World(agents, player, viewPort)

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
