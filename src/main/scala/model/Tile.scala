package me.mtrupkin.game.model

import me.mtrupkin.console.Colors._
import me.mtrupkin.console.{Screen, ScreenChar}
import me.mtrupkin.core.{Matrix, Point, Size}
import me.mtrupkin.game.model.World._
import rexpaint.RexPaintImage

import scala.Array._
import scala.collection.mutable.HashMap

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

  def apply(p: Point): Tile = {
    val (section, tile) = toLocal(p)
    val tileMap = tileMaps(section)
    tileMap(tile)
  }

  def move(p: Point): Boolean = {
    val (section, tile) = toLocal(p)
    val tileMap = tileMaps(section)
    tileMap.move(tile)
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
      case ' ' | '.' => new Floor
      case _ => new Wall(s)
    }
  }
}

object TileMap {

  // create tile map from a matrix in memory
  def load(levelName: String, matrix: Matrix[ScreenChar]): MutableTileMap = {
    val tileMap = new MutableTileMap(levelName, matrix.size)
    matrix.foreach((p: Point, sc: ScreenChar) => tileMap(p) = sc)

    tileMap
  }

  def load(levelName: String, size: Size, matrix: Seq[Seq[ScreenChar]]): MutableTileMap = {
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
}


