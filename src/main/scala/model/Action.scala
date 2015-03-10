package me.mtrupkin.game.model

import java.nio.file.{Files, Paths}

import me.mtrupkin.core.Point
import me.mtrupkin.pathfinding.{Dijkstra}

import scala.io.Source

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

object SVGIcons {
  import scala.collection.JavaConverters._

  val url = getClass.getResource("/views/svg-icons.txt")
  val path = Paths.get(url.toURI)
  val lines = Files.readAllLines(path).asScala

  def parse(lines: List[String], acc: List[(String, String)]): Seq[(String, String)] = {
    lines match {
      case l1 :: l2 :: ls => parse(ls, (l1, l2) :: acc)
      case Nil => acc
    }
  }

  val iconMap = parse(lines.toList, Nil).toMap

  def svg(icon: String): String = iconMap.get(icon).get
}

trait ActionOption {
  def name: String
  def icon: Option[String]
  var selected = false
  def getAction(target: Point): Option[Action]
}

object BasicActionOption {
  def getAttack(tracker: CombatTracker, newTarget: Point, range: Int): Option[Action] = {
    import tracker._
    for {
      a <- agents.find(a => a.position == newTarget)
    }  {
      val los = lineOfSight(a.position, player.position)

      return if (los.length > 0 && los.length <= range) Some(new AttackAction(tracker.world.player, a)) else None
    }

    None
  }

  def getMove(tracker: CombatTracker, newTarget: Point, range: Int): Option[Action] = {
    import tracker._
    val moves = tracker.pathFinder.moveCount(newTarget, player.position, range)
    val path = tracker.pathFinder.path(newTarget, player.position)
    if ((moves > 0) && (moves <= range) && (path != Nil) && agents.forall(_.position != newTarget))
      Some(new MoveAction(tracker.world.player, newTarget))
    else None
  }
}

class MoveOption(val tracker: CombatTracker, val name: String = "Move") extends ActionOption {
  val icon = None
  selected = true

  def getAction(target: Point): Option[Action] = {
    BasicActionOption.getMove(tracker, target, tracker.player.move)
  }
}

class AttackOption(val tracker: CombatTracker, val name: String = "Attack") extends ActionOption {
  val icon = None
  selected = true

  def getAction(target: Point): Option[Action] = {
    BasicActionOption.getAttack(tracker, target, tracker.player.range)
  }
}

class BurstAttackOption(val tracker: CombatTracker, val name: String = "Burst Attack") extends ActionOption {
  val icon = Some("lightning")
  def getAction(target: Point): Option[Action] = {
    BasicActionOption.getAttack(tracker, target, (tracker.player.range * 1).toInt)
  }
}

class AimAttackOption(val tracker: CombatTracker, val name: String = "Aim Attack") extends ActionOption {
  val icon = Some("arrow-lines")
  def getAction(target: Point): Option[Action] = {
    BasicActionOption.getAttack(tracker, target, tracker.player.range * 4)
  }
}