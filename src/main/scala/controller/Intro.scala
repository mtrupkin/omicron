package me.mtrupkin.controller


import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.Pane

import me.mtrupkin.console.{ConsoleKey, Modifiers}
import me.mtrupkin.core.{Point, Points, Size}
import me.mtrupkin.game.model._

import scalafx.Includes._
import scalafx.scene.{control => sfxc, input => sfxi, layout => sfxl}


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
      val (startLevel, _) = Tile.loadStartTile("start")

      changeState(new GameController(new CombatTracker(world, Size(startLevel.size.width*3, startLevel.size.height))))
    }



    def handleNewGame(event: ActionEvent) = {

      // create world
      val levelPosition2 = Point(1,0)
      val levelPosition3 = Point(2,0)
      val (startLevel, player) = Tile.loadStartTile("start")
      val (level2, agents2) = Tile.loadTile("tile-2")
      val (level3, agents3) = Tile.loadTile("tile-3")

      val levelSize = startLevel.size
      val levelMap = new Level("mission-1", levelSize)
      levelMap.tiles(Points.Origin) = startLevel
      levelMap.tiles(levelPosition2) = level2
      levelMap.tiles(levelPosition3) = level3

      levelMap.agents = levelMap.toWorldAgent(levelPosition2, agents2) ++ levelMap.toWorldAgent(levelPosition3, agents3)

      val world = new World(player, levelMap)

      World.write(world)

      val viewSize = Size(levelSize.width * 3, levelSize.height)
      changeState(new GameController(new CombatTracker(world, viewSize)))
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
