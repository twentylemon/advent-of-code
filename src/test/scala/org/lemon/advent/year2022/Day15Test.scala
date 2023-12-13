package org.lemon.advent.year2022

import cats._
import cats.implicits._
import cats.collections._
import cats.collections.syntax.all._

import org.lemon.advent._

class Day15Test extends UnitTest {

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2
    def manhattan(rhs: Coord) = (coord.x - rhs.x).abs + (coord.y - rhs.y).abs

  case class Sensor(sensor: Coord, beacon: Coord):
    val distance = sensor manhattan beacon

  def parseSensor(line: String) = line match
    case s"Sensor at x=$xs, y=$ys: closest beacon is at x=$xb, y=$yb" =>
      Sensor(sensor = (xs.toInt, ys.toInt), beacon = (xb.toInt, yb.toInt))

  def coverage(sensor: Sensor, row: Int) =
    val diff = (sensor.sensor.y - row).abs
    if diff <= sensor.distance then
      (sensor.sensor.x - sensor.distance + diff) toIncl (sensor.sensor.x + sensor.distance - diff)
    else 0 toExcl 0

  def coverageTree(sensors: Seq[Sensor], row: Int) =
    sensors.foldLeft(Diet.empty[Int])((diet, sensor) => diet + coverage(sensor, row))

  def part1(in: Seq[String], row: Int) =
    val sensors = in.map(parseSensor)
    val diet = coverageTree(sensors, row)
    coverageTree(sensors, row).toIterator.map(range => range.end - range.start).sum

  def part2(in: Seq[String], range: Range[Int]): Long =
    val sensors = in.map(parseSensor)
    val (diet, row) =
      range.toIterator.map(coverageTree(sensors, _)).zipWithIndex.dropWhile(_._1.containsRange(range)).next
    (diet.toIterator.next.end + 1) * 4000000L + row

  test("part 1 example") {
    val in = """|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

    part1(in.linesIterator.toSeq, 10) shouldBe 26
  }

  test("part 1") {
    part1(readLines(file(2022)(15)), 2000000) shouldBe 5607466
  }

  test("part 2 example") {
    val in = """|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

    part2(in.linesIterator.toSeq, 0 toIncl 20) shouldBe 56000011L
  }

  test("part 2") {
    part2(readLines(file(2022)(15)), 0 toIncl 4000000) shouldBe 12543202766584L
  }

}
