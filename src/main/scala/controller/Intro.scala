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

      changeState(new GameController(new CombatTracker(world, World.viewSize)))
    }



    def handleNewGame(event: ActionEvent) = {
      // create world
      val world = World.loadMission("mission-1")

      World.write(world)

      changeState(new GameController(new CombatTracker(world, World.viewSize)))
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
