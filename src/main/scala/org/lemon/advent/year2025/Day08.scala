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
    circuits.components.map(_.size).toSeq.sorted.reverse.take(3).product

  def minimumSpanningTree(coords: Seq[Coord]): Seq[(Coord, Coord)] =
    val (_, tree) = coords.pairs.toVector
      .sortBy(_.euclidean(_))
      .foldLeft((DisjointSet(coords), Vector.empty[(Coord, Coord)])) { case ((circuits, tree), (a, b)) =>
        if circuits.connected(a, b) then (circuits, tree)
        else (circuits.union(a, b), tree :+ (a, b))
      }
    tree

  def part2(input: String) =
    val coords = parse(input)
    val mst = minimumSpanningTree(coords)
    val lastEdge = mst.sortBy(_.euclidean(_)).last
    lastEdge._1.x * lastEdge._2.x
