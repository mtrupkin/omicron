package me.mtrupkin.game.model

import java.io._
import java.nio.file.{StandardOpenOption, Path, Paths, Files}

import me.mtrupkin.console.{ScreenChar, Colors, RGB, Screen}
import me.mtrupkin.core.{Matrix, Point, Size}
import play.api.libs.json._
import rexpaint.RexPaintImage


/**
 * Created by mtrupkin on 12/19/2014.
 */
class ViewPort(val size: Size, var origin: Point, levelMap: LevelMap) extends TileMap {
  val levelName = levelMap.levelName

  def render(screen: Screen): Unit =
    size.foreach(p => {
      val world = toWorld(p)
      screen(p) = this(world).sc
    })
  // in screen coordinates
  def apply(p: Point): Tile = levelMap(toWorld(p))
  // in screen coordinates
  def move(p: Point): Boolean = levelMap.move(toWorld(p))

  // in world coordinates
  def explore(p: Point): Seq[Agent] = {
    levelMap.explore(p: Point) match {
      case Some((direction, agents: Seq[Agent])) => {
        origin += Point( direction.x * (size.width/2 - 1), direction.y * (size.height/2 - 1))
        agents
      }
      case _ => Nil
    }
  }
  // in world coordinates
  def render(screen: Screen, p0: Point, sc: ScreenChar): Unit = {
    val p = toScreen(p0)
    if (screen.size.in(p))
      screen(p) = sc
  }

  def renderAgent(screen: Screen, agent: Entity, distance: Int): Unit = {
    render(screen, agent.position, agent.sc)
  }

  def toScreen(p0: Point): Point = p0 - origin
  def toWorld(p0: Point): Point = p0 + origin
}

class World (
  var agents: Seq[Agent],
  val player: Player,
  val viewPort: ViewPort,
  var time: Long = 0)  {

  def update(elapsed: Int) {
    time += elapsed
  }

  def render(screen: Screen): Unit = {
    viewPort.render(screen)

    renderAgent(screen, player, 0)
  }

  def renderAgent(screen: Screen, agent: Entity, distance: Int): Unit = viewPort.renderAgent(screen, agent, distance)


  def explore(p: Point): Unit = {
    agents = agents ++ viewPort.explore(p)
  }
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
    new World(worldJS.agents.map(toAgent(_)), toPlayer(worldJS.player), ???, worldJS.time)
  }

  def write(world: World): Unit = {
    val worldJS = WorldJS(world.viewPort.levelName, world.agents.map(toAgentJS(_)), toPlayerJS(world.player), world.time)
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
