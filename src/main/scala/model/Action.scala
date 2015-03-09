package me.mtrupkin.game.model

import me.mtrupkin.core.Point
import me.mtrupkin.pathfinding.{Dijkstra}

/**
 * Created by mtrupkin on 1/17/2015.
 */
trait Action {
  def name: String
}

case class MoveAction(entity: Entity, target: Point, name: String = "Move") extends Action {
  def complete(): Unit =  {
    entity.ap -= 1
  }
}

case class AttackAction(attacker: Entity, defender: Entity, name: String = "Attack") extends Action {
  def complete(): Unit =  {
    attacker.ap -= 1
    Combat.attack(attacker.melee, defender, Combat.playerAttack)
  }
}

trait ActionOption {
  def getAction(target: Point): Option[Action]
}

class MoveOption(val tracker: CombatTracker, val name: String = "Move") extends ActionOption {
  import tracker._

  def getAction(target: Point): Option[Action] = {
    val move = player.move
    val moves = tracker.pathFinder.moveCount(target, player.position, move)
    val path = tracker.pathFinder.path(target, player.position)
    if ((moves > 0) && (moves <= move) && (path != Nil) && agents.forall(_.position != target))
      Some(new MoveAction(tracker.world.player, target))
    else None
  }
}


class AttackOption(val tracker: CombatTracker, val name: String = "Attack") extends ActionOption {
  import tracker._

  def getAction(target: Point): Option[Action] = {
    for {
      a <- agents.find(a => a.position == target)
    } {
      val los = lineOfSight(a.position, player.position)
      if (los != Nil) return Some(new AttackAction(tracker.world.player, a))
    }
    None
  }
}