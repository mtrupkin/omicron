package me.mtrupkin.pathfinding


import me.mtrupkin.core.{Points, Size, Point, Matrix}
import me.mtrupkin.game.model.TileMap

import scala.collection.mutable

/**
 * Created by mtrupkin on 1/2/2015.
 */
class Dijkstra(val tileMap: TileMap) {
  protected case class Node(p: Point, weight: Int = 1, dist: Double = Double.MaxValue) extends Ordered[Node] {
    override def compare(o: Node): Int = (o.dist-dist).toInt
    override def toString: String = s"$p dist: $dist"
  }
  protected val size = tileMap.size
  protected val nodes = new Matrix[Node](size)
  protected var start: Point = _

  // dijkstra's algorithm using a binary heap.
  protected def search(p: Point, r: Int): Unit = {
    var q = new mutable.PriorityQueue[Node]()
    start = p
    size.foreach(p => nodes(p) = Node(p)) // optimization candidate

    // add source node
    val source = nodes(p).copy(dist = 0)
    nodes(p) = source
    q += source

    while (!q.isEmpty) {
      // node with shortest distance
      val u = q.dequeue()

      // look at each neighbour
      for {
        v <- neighbours(u.p)
        newDist = u.dist + v.weight
        if ((newDist < v.dist) && (newDist <= r))
      } {
        // shorter path to neighbour found
        val newNode = v.copy(dist = newDist)
        nodes(v.p) = newNode
        q += newNode
      }
    }
  }

  protected def neighbours(p: Point): Seq[Node] = {
    for {
      n <- size.neighbors(p)
      if tileMap.move(n)
    } yield nodes(n)
  }

  protected def path(p: Point, acc: List[Point]): Seq[Point] = {
    val dist = nodes(p).dist

    if (dist == 0) return acc

    val ns = for {
      n <- size.neighbors(p)
      if (nodes(n).dist < dist)
    } yield n

    if (ns != Nil) path(ns.head, p :: acc) else Nil
  }

  def moveCount(p: Point, p0: Point, r: Int): Int = {
    if (!size.in(p)) return Int.MaxValue
    if (p0 != start) search(p0, r)
    nodes(p).dist.toInt
  }

  def path(p: Point, p0: Point): Seq[Point] = if (tileMap.size.in(p)) path(p, Nil) else Nil
}
