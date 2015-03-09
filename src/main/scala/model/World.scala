package me.mtrupkin.game.model

import java.io._
import java.nio.file.{StandardOpenOption, Path, Paths, Files}

import me.mtrupkin.console.Screen
import me.mtrupkin.core.{Point, Size}
import play.api.libs.json._
import rexpaint.RexPaintImage


/**
 * Created by mtrupkin on 12/19/2014.
 */
class World (
  val agents: Seq[Agent],
  val player: Player,
  var tileMap: TileMap,
  var time: Long = 0)  {

  def update(elapsed: Int) {
    time += elapsed
  }

  def render(screen: Screen): Unit = {
    tileMap.render(screen)

    renderAgent(screen, player)
  }

  def renderAgent(screen: Screen, agent: Entity): Unit = screen.write(agent.position, agent.sc)
}

case class EntityJS(name: String, position: Point, hp: Int)

case class WorldJS(levelName: String, agents: Seq[EntityJS], player: EntityJS, time: Long)

object World {
  val saveDirectory = Paths.get("./save")
  val savePath = saveDirectory.resolve("game.json")

  import scala.collection.JavaConversions._
  implicit val formatAgent = Json.format[EntityJS]
  implicit val formatWorld = Json.format[WorldJS]

  def read(): World = {
    val is = Files.newInputStream(savePath)
    val json = Json.parse(is)
    val worldJS = Json.fromJson[WorldJS](json).get
    val tileMap = TileMap.load(worldJS.levelName)
    new World(worldJS.agents.map(toAgent(_)), toPlayer(worldJS.player), tileMap, worldJS.time)
  }

  def write(world: World): Unit = {
    val worldJS = WorldJS(world.tileMap.levelName, world.agents.map(toAgentJS(_)), toPlayerJS(world.player), world.time)
    val json = Json.toJson(worldJS)

    Files.createDirectories(saveDirectory)
    Files.write(savePath, Seq(Json.prettyPrint(json)), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  def exists(): Boolean = Files.exists(savePath)

  def delete(): Unit = Files.delete(savePath)

  protected def toAgentJS(agent: Agent): EntityJS = {
    EntityJS(agent.name, agent.position, agent.hp)
  }

  protected def toAgent(agentJS: EntityJS): Agent = {
    agentJS.name match {
      case "Turret" => new Agent (agentJS.name, 'T', agentJS.position, currentHP = Some(agentJS.hp) )
      case _ =>  ???
    }
  }

  protected def toPlayerJS(agent: Player): EntityJS = {
    EntityJS(agent.name, agent.position, agent.hp)
  }

  protected def toPlayer(agentJS: EntityJS): Player = {
    new Player(agentJS.name, '@', agentJS.position, currentHP = Some(agentJS.hp))
  }
}
