package me.mtrupkin.game.model

import java.nio.file.{Files, Paths}

import me.mtrupkin.core.Point

/**
 * Created by mtrupkin on 1/17/2015.
 */


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

trait Actions {
  self: CombatTracker =>

  trait ActionOption {
    def name: String
    def icon: Option[String]

    var selected = false
    var ready = true
    var target: Point = _
    var cooldown = 0
    var maxCooldown = 0

    def entity: Entity
    def canAct(target: Point): Boolean = { selected && ready && canPerformAction(target) }
    def act(target: Point): Unit = {
      this.target = target
      performAction()
    }

    def complete(): Unit = {
      if (maxCooldown > 0){
        ready = false
        cooldown = maxCooldown
      }
      completeAction()
    }

    def endRound(): Unit = {
      if (cooldown > 0) {
        cooldown -= 1
      }
      if (cooldown == 0) ready = true
    }

    protected def canPerformAction(target: Point): Boolean
    protected def performAction(): Unit
    protected def completeAction(): Unit

    def canAttack(target: Point, range: Int): Boolean = {
      for {
        a <- agents.find(a => a.position == target)
      } {
        val los = lineOfSight(a.position, entity.position)
        return (los.length > 0 && los.length <= range)
      }

      false
    }

    def canMove(target: Point, range: Int): Boolean = {
      val moves = pathFinder.moveCount(target, entity.position, range)
      val path = pathFinder.path(target, entity.position)
      ((moves > 0) && (moves <= entity.move) && (path != Nil) && agents.forall(_.position != target))
    }
  }

  class MoveOption(val entity: Entity, val name: String = "Move") extends ActionOption {
    val icon: Option[String] = None
    selected = true

    def canPerformAction(target: Point): Boolean = canMove(target, entity.move)
    def performAction(): Unit = {}
    def completeAction(): Unit =  {
      entity.ap -= 1
      world.explore(player.position)
    }
  }

  class AttackOption(val entity: Entity, val name: String = "Attack") extends ActionOption {
    val icon: Option[String] = None
    selected = true

    var defender: Entity = _

    def canPerformAction(target: Point): Boolean = canAttack(target, entity.range)

    def performAction(): Unit = {
      for {
        a <- agents.find(a => a.position == target)
      } {
        defender = a
      }
    }

    def completeAction(): Unit =  {
      entity.ap -= 1
      Combat.attack(entity.melee, defender, Combat.playerAttack)
    }
  }

  class BurstAttackOption(entity: Entity) extends AttackOption(entity, "Burst Attack") {
    override val icon = Some("lightning")
    selected = false
    maxCooldown = 1

    override def canPerformAction(target: Point): Boolean = canAttack(target, entity.range * 2)
  }

  class AimAttackOption(entity: Entity) extends AttackOption(entity, "Aim Attack") {
    override val icon = Some("arrow-lines")
    selected = false
    maxCooldown = 2

    override def canPerformAction(target: Point): Boolean = canAttack(target, entity.range * 5)
  }
}