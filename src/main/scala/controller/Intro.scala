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

    def loadImageLevel(levelName: String): (RexPaintImage, MutableTileMap) = {
      val is = getClass.getResourceAsStream(s"/levels/$levelName.xp")
      val image = RexPaintImage.read(levelName, is)
      val level = TileMap.load(levelName, image.size, image.layers.head.matrix)
      (image, level)
    }

    def loadLevel(levelName: String): (MutableTileMap, Seq[Agent]) = {
      val (image, level) = loadImageLevel(levelName)

      val agents = Entity.toEntities(image.layers(1).matrix)
      (level, agents)
    }

    def loadStartLevel(levelName: String): (MutableTileMap, Player) = {
      val (image, level) = loadImageLevel(levelName)
      val player = Entity.toPlayer(image.layers(1).matrix)
      (level, player)
    }

    def handleNewGame(event: ActionEvent) = {

      // create world
      val (startLevel, player) = loadStartLevel("start")
      val (level2, agents2) = loadLevel("tile-2")
      val (level3, agents3) = loadLevel("tile-3")

      val agents = agents2 ++ agents3

      val levelSize = startLevel.size
      val levelMap = new LevelMap("mission-1", levelSize)
      levelMap.tileMaps(Points.Origin) = startLevel
      levelMap.tileMaps(Point(1,0)) = level2
      levelMap.tileMaps(Point(2,0)) = level3

      val viewPort = new ViewPort(Size(levelSize.width * 3, levelSize.height), Points.Origin, levelMap)
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
