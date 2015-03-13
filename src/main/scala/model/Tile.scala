package me.mtrupkin.game.model

import me.mtrupkin.console.Colors._
import me.mtrupkin.console.{Screen, ScreenChar}
import me.mtrupkin.core.{Points, Matrix, Point, Size}
import me.mtrupkin.game.model.World._
import rexpaint.RexPaintImage

import scala.Array._
import scala.collection.mutable.HashMap
import scala.util.Random

/**
 * Created by mtrupkin on 12/14/2014.
 */
trait Tile {
  def name: String
  def sc: ScreenChar
  def move: Boolean
}

trait TileMap {
  def levelName: String
  def apply(p: Point): Tile
  def move(p: Point): Boolean
}

class MutableTileMap(val levelName: String, val size: Size) extends TileMap {
  val tiles = ofDim[Tile](size.width, size.height)

  def apply(p: Point): Tile = tiles(p.x)(p.y)
  def update(p: Point, value: Tile): Unit = tiles(p.x)(p.y) = value
  def move(p: Point): Boolean = size.in(p) && this(p).move

}

class LevelMap(val levelName: String, val tileMapSize: Size) extends TileMap {
  val tileMaps = new HashMap[Point, TileMap]

  def toLocal(p: Point): (Point, Point) = {
    val section = Point(p.x / tileMapSize.width, p.y / tileMapSize.height)
    val tile = Point(p.x % tileMapSize.width, p.y % tileMapSize.height)
    (section, tile)
  }

  def toGlobal(section: Point, tile: Point): Point = {
    Point((section.x * tileMapSize.width) + tile.x, (section.y * tileMapSize.height) + tile.y)
  }

  def toGlobalAgent(levelPosition: Point, agents: Seq[Agent]): Seq[Agent] = {
    for {
      agent <- agents
    } yield {
      agent.position = toGlobal(levelPosition, agent.position)
      agent
    }
  }


  def apply(p: Point): Tile = {
    val (section, tile) = toLocal(p)
    val tileMapOpt = tileMaps.get(section)

    val res = for {
      tileMap <- tileMapOpt
    } yield tileMap(tile)

    res.getOrElse(Tile.unexplored)
  }

  def move(p: Point): Boolean = {
    val (section, tile) = toLocal(p)
    val tileMap = tileMaps(section)
    tileMap.move(tile)
  }

  def explore(p: Point): Option[(Point, Seq[Agent])] = {
    val (section, tile) = toLocal(p)
    val directionOpt = getEdge(tile)

    for {
      direction <- directionOpt
    } {
      tileMaps.get(section+direction) match {
        case None => {
          val n = Random.nextInt(6) + 1
          val (level, agents) = TileMap.loadLevel(s"tile-$n")
          val newSection = section + direction
          tileMaps(newSection) = level
          return Some((direction, toGlobalAgent(newSection, agents)))
        }
        case Some(tileMap) => Some((direction, Nil))
      }
    }
    None
  }

  def getEdge(p: Point): Option[Point] = {
    val Height = (tileMapSize.height - 1)
    val Width = (tileMapSize.width -1)
    p match {
      case Point(0, _) => Some(Points.Left) //Point(-1, 0)
      case Point(_, 0) => Some(Points.Up) //Point(0, -1)
      case Point(_, Height) => Some(Points.Down) //Point(0, +1)
      case Point(Width, _) => Some(Points.Right) //Point(+1, +1)
      case _ => None
    }
  }
}

class Floor extends Tile {
  val name = "Floor"
  val move = true
  var sc = ScreenChar(' ', fg = LightGrey)
}

class Wall(val sc: ScreenChar) extends Tile {
  val name = "Wall"
  val move = false
}

object Tile {
  implicit def toTile(s: ScreenChar): Tile = {
    s.c match {
      case ' ' | '.' | 'E' => new Floor
      case _ => new Wall(s)
    }
  }
  def floor(): Tile = new Floor
  val unexplored = new Wall(' ')
  def wall(sc: ScreenChar) = new Wall(sc)
}

object TileMap {

  // create tile map from a matrix in memory
  protected def load(levelName: String, matrix: Matrix[ScreenChar]): MutableTileMap = {
    val tileMap = new MutableTileMap(levelName, matrix.size)
    matrix.foreach((p: Point, sc: ScreenChar) => tileMap(p) = sc)

    tileMap
  }

  protected def load(levelName: String, size: Size, matrix: Seq[Seq[ScreenChar]]): MutableTileMap = {
    val tileMap = new MutableTileMap(levelName, size)
    for((i, x) <- matrix.zipWithIndex) {
      for((t, y) <- i.zipWithIndex) {
        tileMap.tiles(x)(y) = t
      }
    }

    tileMap
  }

  // create tile map from first layer of rex paint image
  def load(levelName: String): MutableTileMap = {
    val is = getClass.getResourceAsStream(s"/levels/$levelName.xp")
    val image = RexPaintImage.read(levelName, is)
    load(levelName, image.size, image.layers.head.matrix)
  }

  protected def loadImageLevel(levelName: String): (RexPaintImage, MutableTileMap) = {
    val is = getClass.getResourceAsStream(s"/levels/$levelName.xp")
    val image = RexPaintImage.read(levelName, is)
    val level = load(levelName, image.size, image.layers.head.matrix)
    (image, level)
  }

  def loadLevel(levelName: String): (MutableTileMap, Seq[Agent]) = {
    val (image, level) = loadImageLevel(levelName)

    val agents = Entity.toEntities(image.layers(1).matrix)
    (level, agents)
  }

  def loadStartLevel(levelName: String): (MutableTileMap, Player) = {
    val (image, level) = loadImageLevel(levelName)
    val player = Entity.toPlayer(image.layers(1).matrix)
    (level, player)
  }
}


