package org.lemon.advent.year2022

import org.lemon.advent.lib.geometry.*
import org.lemon.advent.lib.graph.*

private object Day18:

  def parseCube(line: String) = line match
    case s"$x,$y,$z" => Point3(x.toInt, y.toInt, z.toInt)

  def part1(in: String) =
    val cubes = in.linesIterator.map(parseCube).toSet
    cubes.toSeq.map(_.adjacent.count(!cubes.contains(_))).sum

  def part2(in: String) =
    val cubes = in.linesIterator.map(parseCube).toSet
    val box = Cuboid(cubes).expand(1)
    val air = fill((coord: Point3[Int]) => coord.adjacent.filter(box.contains).filterNot(cubes), box.topLeftFront)
    cubes.toSeq.map(_.adjacent.count(air)).sum
