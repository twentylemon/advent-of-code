package org.lemon.advent.year2022

import scala.collection.mutable
import org.lemon.advent._

class Day18Test extends UnitTest {

  type Coord = (Int, Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2
    def z = coord._3
    def left = coord.copy(_1 = coord.x - 1)
    def right = coord.copy(_1 = coord.x + 1)
    def up = coord.copy(_2 = coord.y - 1)
    def down = coord.copy(_2 = coord.y + 1)
    def in = coord.copy(_3 = coord.z - 1)
    def out = coord.copy(_3 = coord.z + 1)
    def cardinals = Set(coord.left, coord.right, coord.up, coord.down, coord.in, coord.out)

  type Face = (Double, Double, Double)

  def parseCube(line: String) = line match
    case s"$x,$y,$z" => (x.toInt, y.toInt, z.toInt)

  def faces(cube: Coord): Set[Face] = Set(
    (cube.x + 0.5, cube.y, cube.z),
    (cube.x - 0.5, cube.y, cube.z),
    (cube.x, cube.y + 0.5, cube.z),
    (cube.x, cube.y - 0.5, cube.z),
    (cube.x, cube.y, cube.z + 0.5),
    (cube.x, cube.y, cube.z - 0.5)
  )

  def faces(cubes: Iterable[Coord]): Map[Face, Int] = cubes.view.flatMap(faces).groupBy(x => x).mapValues(_.size).toMap

  def part1(in: Seq[String]) = faces(in.map(parseCube)).values.count(_ == 1)

  def part2(in: Seq[String]) =
    val cubes = in.map(parseCube).toSet
    val (xBox, yBox, zBox) = (
      cubes.map(_.x).min - 1 to cubes.map(_.x).max + 1,
      cubes.map(_.y).min - 1 to cubes.map(_.y).max + 1,
      cubes.map(_.z).min - 1 to cubes.map(_.z).max + 1
    )
    def inBoundingBox(coord: Coord) = xBox.contains(coord.x) && yBox.contains(coord.y) && zBox.contains(coord.z)

    val air = mutable.Set((xBox.min, yBox.min, zBox.min))
    val queue = air.to(mutable.Queue)
    while !queue.isEmpty do
      val cube = queue.dequeue
      val adjacentAir = cube.cardinals.filter(inBoundingBox) -- air -- cubes
      air ++= adjacentAir
      queue ++= adjacentAir

    (faces(air).keySet & faces(cubes).keySet).size

  test("part 1 combined cubes") {
    part1(Seq("1,1,1", "2,1,1")) shouldBe 10
  }

  test("part 1 separate cubes") {
    part1(Seq("1,1,1", "3,1,1")) shouldBe 12
  }

  test("part 1 example") {
    val in = """|2,2,2
                |1,2,2
                |3,2,2
                |2,1,2
                |2,3,2
                |2,2,1
                |2,2,3
                |2,2,4
                |2,2,6
                |1,2,5
                |3,2,5
                |2,1,5
                |2,3,5""".stripMargin
    part1(in.linesIterator.toSeq) shouldBe 64
  }

  test("part 1") {
    part1(readLines(file(2022)(18))) shouldBe 4460
  }

  test("part 2 example") {
    val in = """|2,2,2
                |1,2,2
                |3,2,2
                |2,1,2
                |2,3,2
                |2,2,1
                |2,2,3
                |2,2,4
                |2,2,6
                |1,2,5
                |3,2,5
                |2,1,5
                |2,3,5""".stripMargin
    part2(in.linesIterator.toSeq) shouldBe 58
  }

  test("part 2") {
    part2(readLines(file(2022)(18))) shouldBe 2498
  }
}
