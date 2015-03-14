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
}

case class EntityJS(name: String, position: Point, hp: Int)
case class TileJS(name: String, position: Point)
case class WorldJS(name: String, radius:Int, tiles: Seq[TileJS], agents: Seq[EntityJS], player: EntityJS, time: Long)
case class MissionJS(name: String, description: String, radius:Int, start: TileJS, tiles: Seq[TileJS])

object World {
  val tileSize = Size(25, 25)
  val viewSize = Size(tileSize.width*3, tileSize.height)
  val saveDirectory = Paths.get("./save")
  val savePath = saveDirectory.resolve("game.json")

  import scala.collection.JavaConversions._
  implicit val formatTile = Json.format[TileJS]
  implicit val formatAgent = Json.format[EntityJS]
  implicit val formatWorld = Json.format[WorldJS]
  implicit val formatMission = Json.format[MissionJS]

  def loadMission(name: String): World = {
    val is = getClass.getResourceAsStream(s"/missions/$name.json")
    val json = Json.parse(is)
    val missionJS = Json.fromJson[MissionJS](json).get
    val level = new Level(missionJS.name, tileSize, missionJS.radius)

    val (startTile, player) = Tile.loadStartTile(missionJS.start.name)
    level.tiles(missionJS.start.position) = startTile
//    missionJS.tiles.foreach(t => level.tiles(t.position) = Tile.loadTile(t.name))
    for {
      tileJS <- missionJS.tiles
      position = tileJS.position
      (tile, agents) = Tile.loadTile(tileJS.name)
    }  {
      level.tiles(tileJS.position) = tile
      level.agents = level.agents ++ level.toWorldAgent(position, agents)
    }

    addBorderTiles(level)

    new World(player, level)
  }

  def addBorderTiles(level: Level): Unit = {
    val r = level.maxRadius - 1
    val max = level.maxRadius

    val left = Tile.load("hull-left")
    val right = Tile.load("hull-right")
    val upper = Tile.load("hull-upper")
    val lower = Tile.load("hull-lower")

    val ul = Tile.load("hull-ul")
    val ur = Tile.load("hull-ur")
    val lr = Tile.load("hull-lr")
    val ll = Tile.load("hull-ll")

    for {
      x <- -r to r
    } {
      level.tiles((x, -max)) = upper
      level.tiles((x, max)) = lower
    }

    for {
      y <- -r to r
    } {
      level.tiles((-max, y)) = left
      level.tiles((max, y)) = right
    }

    level.tiles((-max, -max)) = ul
    level.tiles((max, -max)) = ur
    level.tiles((max, max)) = lr
    level.tiles((-max, max)) = ll
  }

  def read(): World = {
    val is = Files.newInputStream(savePath)
    val json = Json.parse(is)
    val worldJS = Json.fromJson[WorldJS](json).get
    val level = new Level(worldJS.name, tileSize, worldJS.radius)
    level.agents = worldJS.agents.map(a => toAgent(a))

    worldJS.tiles.foreach(t => level.tiles(t.position) = Tile.load(t.name))

    new World(toPlayer(worldJS.player), level, worldJS.time)
  }

  def write(world: World): Unit = {
    val tilesJS = for {
      (p, tileMap) <- world.levelMap.tiles
    } yield TileJS(tileMap.name, p)

    val worldJS = WorldJS(world.levelMap.name, world.levelMap.maxRadius, tilesJS.toSeq, world.levelMap.agents.map(toAgentJS(_)), toPlayerJS(world.player), world.time)
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
