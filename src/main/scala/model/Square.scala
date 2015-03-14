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
trait Square {
  def name: String
  def sc: ScreenChar
  def move: Boolean
}

trait GameMap {
  def apply(p: Point): Square
}

class Tile(val name: String, val size: Size) extends GameMap {
  val squares = ofDim[Square](size.width, size.height)

  def apply(p: Point): Square = squares(p.x)(p.y)
  def update(p: Point, value: Square): Unit = squares(p.x)(p.y) = value
}

class Level(val name: String, val tileSize: Size, val maxRadius: Int) extends GameMap {
  var agents: Seq[Agent] = Nil
  val tiles = new HashMap[Point, Tile]

  def apply(p: Point): Square = {
    val (tilePoint, squarePoint) = toLocal(p)
    tiles.get(tilePoint) match {
      case Some(tile) => tile(squarePoint)
      case None => explore(tilePoint)(squarePoint)
    }
  }

  def toLocal(p: Point): (Point, Point) = {
    val tile = Point(p.x / tileSize.width, p.y / tileSize.height)
    val square = Point(p.x % tileSize.width, p.y % tileSize.height)
    toQuadrant(tile, square)
  }

//  def toPositiveQuadrant(tile: Point): (Point, Point) = {
//
//  }
  def toQuadrant(tile: Point, square: Point): (Point, Point) = {
    def sign(x: Int): Int = if (x < 0) -1 else 0

    val dir = Point(sign(square.x), sign(square.y))
    val newSection = tile + dir

    val newTile = dir match {
      case Point(-1, -1) => square + Point(tileSize.width, tileSize.height)
      case Point(-1, 0) => square + Point(tileSize.width, 0)
      case Point(0, -1) => square + Point(0, tileSize.height)
      case Point(0, 0) => square
    }

    (newSection, newTile)
  }

  def toWorld(tile: Point, square: Point): Point = {
    Point((tile.x * tileSize.width) + square.x, (tile.y * tileSize.height) + square.y)
  }

  def toWorldAgent(tile: Point, agents: Seq[Agent]): Seq[Agent] = {
    for {
      agent <- agents
    } yield {
      agent.position = toWorld(tile, agent.position)
      agent
    }
  }

  def explore(tilePoint: Point): Tile = {
    if ((Math.abs(tilePoint.x) <= maxRadius) && (Math.abs(tilePoint.y) <= maxRadius)) {
      val n = Random.nextInt(6) + 1
      val (tile, newAgents) = Tile.loadTile(s"tile-$n")
      tiles(tilePoint) = tile
      agents = agents ++ toWorldAgent(tilePoint, newAgents)

      tile
    } else Tile.space

  }
}

class Floor extends Square {
  val name = "Floor"
  val move = true
  var sc = ScreenChar(' ', fg = LightGrey)
}

class Wall(val sc: ScreenChar) extends Square {
  val name = "Wall"
  val move = false
}

class Space extends Square {
  val name = "Space"
  val move = false
  var sc = ScreenChar(' ', fg = LightGrey)
}

object Square {
  implicit def toTile(s: ScreenChar): Square = {
    s.c match {
      case ' ' | '.' | 'E' => new Floor
      case _ => new Wall(s)
    }
  }
  def floor(): Square = new Floor
  val unexplored = new Wall(' ')
  def wall(sc: ScreenChar) = new Wall(sc)
}

object Tile {
  val space: Tile = {
    val tile = new Tile("Space", tileSize)
    tileSize.foreach(p => tile(p) = new Space )
    tile
  }

  protected def readRexImage(name: String): RexPaintImage = {
    val is = getClass.getResourceAsStream(s"/tiles/$name.xp")
    RexPaintImage.read(name, is)
  }

  protected def load(name: String, size: Size, matrix: Seq[Seq[ScreenChar]]): Tile = {
    val tile = new Tile(name, size)
    for((i, x) <- matrix.zipWithIndex) {
      for((t, y) <- i.zipWithIndex) {
        tile.squares(x)(y) = t
      }
    }
    tile
  }
  
  protected def loadImageTile(name: String): (RexPaintImage, Tile) = {
    val image = readRexImage(name)
    val tile = load(name, image.size, image.layers.head.matrix)
    (image, tile)
  }

  // create tile map from first layer of rex paint image
  // used for loading game
  def load(name: String): Tile = {
    val image = readRexImage(name)
    load(name, image.size, image.layers.head.matrix)
  }

  def loadTile(levelName: String): (Tile, Seq[Agent]) = {
    val (image, tile) = loadImageTile(levelName)

    val agents = Entity.toEntities(image.layers(1).matrix)
    (tile, agents)
  }

  def loadStartTile(name: String): (Tile, Player) = {
    val (image, tile) = loadImageTile(name)
    val player = Entity.toPlayer(image.layers(1).matrix)
    (tile, player)
  }
}


