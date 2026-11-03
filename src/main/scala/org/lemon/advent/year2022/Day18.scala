package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.Point3
import org.lemon.advent.lib.graph.*

private object Day18:

  type Face = (Double, Double, Double)

  def parseCube(line: String) = line match
    case s"$x,$y,$z" => Point3(x.toInt, y.toInt, z.toInt)

  def faces(cube: Point3[Int]): Set[Face] = Set(
    (cube.x + 0.5, cube.y, cube.z),
    (cube.x - 0.5, cube.y, cube.z),
    (cube.x, cube.y + 0.5, cube.z),
    (cube.x, cube.y - 0.5, cube.z),
    (cube.x, cube.y, cube.z + 0.5),
    (cube.x, cube.y, cube.z - 0.5)
  )

  def faces(cubes: Iterable[Point3[Int]]): Map[Face, Int] = cubes.view.flatMap(faces).frequencies

  def part1(in: String) = faces(in.linesIterator.map(parseCube).toSeq).values.count(_ == 1)

  def part2(in: String) =
    val cubes = in.linesIterator.map(parseCube).toSet
    val (xBox, yBox, zBox) = (
      cubes.map(_.x).min - 1 to cubes.map(_.x).max + 1,
      cubes.map(_.y).min - 1 to cubes.map(_.y).max + 1,
      cubes.map(_.z).min - 1 to cubes.map(_.z).max + 1
    )
    def inBoundingBox(coord: Point3[Int]) = xBox.contains(coord.x) && yBox.contains(coord.y) && zBox.contains(coord.z)

    val air = fill(
      (coord: Point3[Int]) => coord.adjacent.filter(inBoundingBox).filterNot(cubes),
      Point3(xBox.min, yBox.min, zBox.min)
    )

    (faces(air).keySet & faces(cubes).keySet).size
