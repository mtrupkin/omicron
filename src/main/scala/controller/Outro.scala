package me.mtrupkin.controller

import javafx.scene.input.MouseEvent

import me.mtrupkin.game.model.World


/**
 * Created by mtrupkin on 1/23/2015.
 */
trait Outro {
  self: Controller =>

  class OutroController extends ControllerState {
    val name = "Outro"

    def initialize(): Unit = {
      World.delete()
    }
    def handleMouseClicked(event: MouseEvent) = changeState(new IntroController)
  }

}
