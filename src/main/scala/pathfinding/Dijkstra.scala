package me.mtrupkin.pathfinding


import me.mtrupkin.core.Point
import me.mtrupkin.game.model.GameMap

import scala.collection.mutable

/**
 * Created by mtrupkin on 1/2/2015.
 */
class Dijkstra(val map: GameMap, val maxDist: Int) {
  val maxDim = maxDist * 2

  protected case class Node(p: Point, weight: Int = 1, dist: Double = Double.MaxValue) extends Ordered[Node] {
    override def compare(o: Node): Int = (o.dist-dist).toInt
    override def toString: String = s"$p dist: $dist"
  }

  protected val nodes = new mutable.HashMap[Point, Node]()
  protected var start: Point = _

  // dijkstra's algorithm using a binary heap.
  protected def search(p: Point, r: Int): Unit = {
    var q = new mutable.PriorityQueue[Node]()
    start = p

    foreach(p, maxDist, p => nodes(p) = Node(p))


    // add source node
    val source = nodes(p).copy(dist = 0)
    nodes(p) = source
    q += source

    while (!q.isEmpty) {
      // node with shortest distance
      val u = q.dequeue()

      // look at each neighbour
      for {
        v <- nodeNeighbors(u.p)
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

  protected def nodeNeighbors(p: Point): Seq[Node] = {
    for {
      n <- neighbors(p)
      if map(n).move
    } yield nodes(n)
  }

  protected def path(p: Point, acc: List[Point]): Seq[Point] = {
    val dist = nodes(p).dist

    if (dist == 0) return acc

    val ns = for {
      n <- neighbors(p)
      if (nodes(n).dist < dist)
    } yield n

    if (ns != Nil) path(ns.head, p :: acc) else Nil
  }

  def moveCount(p: Point, p0: Point, r: Int): Int = {
    if (p0 != start) search(p0, r)
    nodes(p).dist.toInt
  }

  def path(p: Point, p0: Point): Seq[Point] = if (nodes.contains(p)) path(p, Nil) else Nil

  def foreach(p: Point, r: Int, f: (Point => Unit)): Unit = {
    for {
      x <- -r to r
      y <- -r to r
    } f(p + (x, y))
  }

  def neighbors(p: Point, r: Int = 1): Seq[Point] = {
    for {
      x <- -r to r
      y <- -r to r
      if !((x == 0) && (y == 0))
      p0 = p + (x, y)
      if nodes.contains(p0)
    } yield p0
  }
}
