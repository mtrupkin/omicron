package me.mtrupkin.game.model

import java.io._
import java.nio.file.{StandardOpenOption, Path, Paths, Files}

import me.mtrupkin.console.{ScreenChar, Colors, RGB, Screen}
import me.mtrupkin.core.{Points, Matrix, Point, Size}
import play.api.libs.json._
import rexpaint.RexPaintImage

import scala.collection.immutable.HashMap


/**
 * Created by mtrupkin on 12/19/2014.
 */

class World (
  val player: Player,
  val levelMap: Level,
  var time: Long = 0)  {

  def update(elapsed: Int) {
    time += elapsed
  }

//  def render(screen: Screen): Unit = {
//    size.foreach(p => {
//      val world = toWorld(p)
//      screen(p) = this(world).sc
//    })
//
//    renderAgent(screen, player, 0)
//  }

//  def renderAgent(screen: Screen, agent: Entity, distance: Int): Unit = viewPort.renderAgent(screen, agent, distance)


  def explore(p: Point): Unit = {
//    levelMap.explore(p: Point) match {
//      case Some((direction, agents: Seq[Agent])) => {
//        origin += Point( direction.x * (size.width/2 - 1), direction.y * (size.height/2 - 1))
//        agents
//      }
//      case _ => Nil
//    }
//    agents = agents ++ viewPort.explore(p)
  }
}

case class EntityJS(name: String, position: Point, hp: Int)
case class TileJS(name: String, position: Point)

case class WorldJS(tiles: Seq[TileJS], agents: Seq[EntityJS], player: EntityJS, time: Long)

object World {
  val saveDirectory = Paths.get("./save")
  val savePath = saveDirectory.resolve("game.json")

  import scala.collection.JavaConversions._
  implicit val formatTile = Json.format[TileJS]
  implicit val formatAgent = Json.format[EntityJS]
  implicit val formatWorld = Json.format[WorldJS]

  def read(): World = {
    val (startLevel, _) = Tile.loadStartTile("start")
    val is = Files.newInputStream(savePath)
    val json = Json.parse(is)
    val worldJS = Json.fromJson[WorldJS](json).get
    val level = new Level("mission-1", startLevel.size)
    level.agents = worldJS.agents.map(a => toAgent(a))

    worldJS.tiles.foreach(t => level.tiles(t.position) = Tile.load(t.name))


    new World(toPlayer(worldJS.player), level, worldJS.time)
  }

  def write(world: World): Unit = {
    val tilesJS = for {
      (p, tileMap) <- world.levelMap.tiles
    } yield TileJS(tileMap.name, p)

    val worldJS = WorldJS(tilesJS.toSeq, world.levelMap.agents.map(toAgentJS(_)), toPlayerJS(world.player), world.time)
    val json = Json.toJson(worldJS)

    Files.createDirectories(saveDirectory)
    Files.write(savePath, Seq(Json.prettyPrint(json)), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  def exists(): Boolean = Files.exists(savePath)

  def delete(): Unit = Files.delete(savePath)

  protected def toAgentJS(agent: Agent): EntityJS = {
    EntityJS(agent.name, agent.position, agent.hp)
  }

  protected def toAgent(agentJS: EntityJS, sc: ScreenChar): Agent = {
    new Agent (agentJS.name, sc, agentJS.position, currentHP = Some(agentJS.hp) )
  }

  protected def toAgent(agentJS: EntityJS): Agent = {
    agentJS.name match {
      case "Turret" => toAgent(agentJS, 'T')
      case "Robot" => toAgent(agentJS, 'R')
      case "Zellan" => toAgent(agentJS, 'Z')
      case "Kaldron" => toAgent(agentJS, 'K')
      case "Cyber Dragon" => toAgent(agentJS, 'C')
      case _ =>  ???
    }
  }

  protected def toPlayerJS(agent: Player): EntityJS = {
    EntityJS(agent.name, agent.position, agent.hp)
  }

  protected def toPlayer(agentJS: EntityJS): Player = {
    new Player(agentJS.name, '@', agentJS.position, stats = Stats(str = 1), currentHP = Some(agentJS.hp))
  }
}
