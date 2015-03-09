package  me.mtrupkin.game.model


import me.mtrupkin.console.{ScreenChar}
import me.mtrupkin.core.{Point, Size}

import scala.collection.mutable.ListBuffer

/**
 * Created by mtrupkin on 12/19/2014.
 */

case class Stats(str: Int = 0, dex: Int = 0, int: Int = 0)

abstract class Entity(currentHP: Option[Int] = None, currentAP: Option[Int] = None) {
  def floor(d: Double): Int = Math.floor(d).toInt

  def name: String
  def sc: ScreenChar
  var position: Point

  var hp: Int = currentHP.getOrElse(maxHP)
  var ap: Int = currentAP.getOrElse(maxAP)

  val stats: Stats
  import stats._

  def maxHP = 1 + (str + dex + int) * 10
  def maxAP = 2

  def move: Int = 4 + floor(dex / 3)

  def melee: Combat = Combat((str + floor(dex/2) + floor(int/3)))
  def ranged: Combat = Combat((dex + floor(str/2) + floor(int/3)))
  def defense: Int = floor((str + dex + int) / 3)

  def endRound(): Unit = {
    ap = maxAP
  }

}

class Agent(
  val name: String,
  val sc: ScreenChar,
  var position: Point,
  val stats: Stats = new Stats,
  currentHP: Option[Int] = None,
  currentAP: Option[Int] = None) extends Entity(currentHP, currentAP) {

  def getAction(tracker: CombatTracker): Option[Action] = {
    val los = tracker.lineOfSight(tracker.world.player.position, position)
    if (los != Nil) return Some(AttackAction(this, tracker.world.player)) else None
  }
}

class Player(
   val name: String,
   val sc: ScreenChar,
   var position: Point,
   val stats: Stats = new Stats,
   currentHP: Option[Int] = None) extends Entity {
}

object Entity {
  def toEntity(sc: ScreenChar, p: Point): Option[Entity] = {
    sc.c match {
      case 'T' => Some(new Agent("Turret", 'T', p))
      case '@' => Some(new Player("Player", '@', p, Stats(str = 1)))
      case _ => None
    }
  }

  def toEntities(matrix: Seq[Seq[ScreenChar]]): (Player, Seq[Agent]) = {
    var player = new Player("Player", '@', Point(1, 1), Stats(str = 1))
    val agents = new ListBuffer[Agent]()

    for {
      (i, x) <- matrix.zipWithIndex
      (t, y) <- i.zipWithIndex
    } toEntity(t, Point(x, y)) match {
      case Some(a: Agent) => agents.append(a)
      case Some(p: Player) => player = p
      case _ =>
    }

    (player, agents)
  }
}