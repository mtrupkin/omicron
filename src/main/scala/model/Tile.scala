package me.mtrupkin.game.model

import me.mtrupkin.console.Colors._
import me.mtrupkin.console.{Screen, ScreenChar}
import me.mtrupkin.core.{Matrix, Point, Size}
import me.mtrupkin.game.model.World._
import rexpaint.RexPaintImage

import scala.Array._

/**
 * Created by mtrupkin on 12/14/2014.
 */
trait Tile {
  def name: String
  def sc: ScreenChar
  def update(elapsed: Int) = {}
  def move: Boolean
}

class TileMap(val levelName: String, val size: Size) {
  val tiles = ofDim[Tile](size.width, size.height)

  def apply(p: Point): Tile = tiles(p.x)(p.y)
  def update(p: Point, value: Tile): Unit = tiles(p.x)(p.y) = value
  def foreach(f: (Point, Tile) => Unit ) = size.foreach(p => f(p, this(p)))

  def move(p: Point): Boolean = size.in(p) && this(p).move
  def moveCost(p: Point): Double = 1
  def update(elapsed: Int): Unit = size.foreach(p => this(p).update(elapsed))

  def render(screen: Screen): Unit = foreach((p, t) => screen(p) = t.sc)
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
  def load(levelName: String, matrix: Matrix[ScreenChar]): TileMap = {
    val tileMap = new TileMap(levelName, matrix.size)
    matrix.foreach((p: Point, sc: ScreenChar) => tileMap(p) = sc)

    tileMap
  }

  def load(levelName: String, size: Size, matrix: Seq[Seq[ScreenChar]]): TileMap = {
    val tileMap = new TileMap(levelName, size)
    for((i, x) <- matrix.zipWithIndex) {
      for((t, y) <- i.zipWithIndex) {
        tileMap.tiles(x)(y) = t
      }
    }

    tileMap
  }

  // create tile map from first layer of rex paint image
  def load(levelName: String): TileMap = {
    val is = getClass.getResourceAsStream(s"/levels/$levelName.xp")
    val image = RexPaintImage.read(levelName, is)
    load(levelName, image.size, image.layers.head.matrix)
  }
}


