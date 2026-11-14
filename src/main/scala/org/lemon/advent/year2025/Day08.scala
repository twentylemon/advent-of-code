package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.Point3
import org.lemon.advent.lib.graph.DisjointSet

private object Day08:

  type Coord = Point3[Long]

  def parse(input: String) =
    input.linesIterator.map(line =>
      val Seq(x, y, z) = line.csv
      Point3(x.toLong, y.toLong, z.toLong)
    ).toSeq

  def part1(input: String, strands: Int = 1000) =
    val coords = parse(input)
    val closests = coords.pairs.toVector
      .sortBy(_.euclidean(_))
      .take(strands)
    val circuits = closests.foldLeft(DisjointSet(coords)) { case (circuits, (a, b)) => circuits.union(a, b) }
    circuits.components.map(_.size).toSeq.sorted.takeRight(3).product

  def minimumSpanningTree(coords: Seq[Coord]): Seq[(Coord, Coord)] =
    val all = coords.toSet
    val initialDist = all.map(_ -> Long.MaxValue).toMap.updated(coords.head, 0L)

    val (_, _, parent) = coords.foldLeft((Set.empty[Coord], initialDist, Map.empty[Coord, Coord])) {
      case ((mst, minDist, parent), _) =>
        val remaining = all -- mst
        val u = remaining.minBy(minDist)
        val neighbours = remaining - u

        val (newDist, newParent) = neighbours.foldLeft((minDist, parent)) {
          case ((dist, par), v) =>
            val d = u.euclidean(v)
            if d < dist(v) then (dist.updated(v, d), par.updated(v, u))
            else (dist, par)
        }

        (mst + u, newDist, newParent)
    }
    coords.tail.map(v => (parent(v), v))

  def part2(input: String) =
    val coords = parse(input)
    val mst = minimumSpanningTree(coords)
    val lastEdge = mst.sortBy(_.euclidean(_)).last
    lastEdge._1.x * lastEdge._2.x
