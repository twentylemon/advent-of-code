package org.lemon.advent.year2022

import org.lemon.advent.lib.*

private object Day15:

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2
    def manhattan(rhs: Coord) = (coord.x - rhs.x).abs + (coord.y - rhs.y).abs

  case class Sensor(sensor: Coord, beacon: Coord):
    val distance = sensor `manhattan` beacon

  def parseSensor(line: String) = line match
    case s"Sensor at x=$xs, y=$ys: closest beacon is at x=$xb, y=$yb" =>
      Sensor(sensor = (xs.toInt, ys.toInt), beacon = (xb.toInt, yb.toInt))

  def coverage(sensor: Sensor, row: Int) =
    val diff = (sensor.sensor.y - row).abs
    if diff <= sensor.distance then
      (sensor.sensor.x - sensor.distance + diff) to (sensor.sensor.x + sensor.distance - diff)
    else 0 until 0

  def coverageTree(sensors: Seq[Sensor], row: Int) =
    sensors.foldLeft(Diet.empty[Int])((diet, sensor) => diet + coverage(sensor, row))

  def part1(input: Seq[String], row: Int) =
    val sensors = input.map(parseSensor)
    val diet = coverageTree(sensors, row)
    diet.intervalsIterator.map((start, end) => end - start).sum

  def part2(input: Seq[String], range: Range): Long =
    val sensors = input.map(parseSensor)
    val (diet, row) = range.iterator.map(coverageTree(sensors, _)).zipWithIndex.dropWhile(_._1(range)).next
    (diet.toIntervals.head.end + 1) * 4000000L + row
