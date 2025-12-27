package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day15:

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

  def part1(input: String, row: Int) =
    val sensors = input.linesIterator.map(parseSensor).toSeq
    val diet = coverageTree(sensors, row)
    diet.intervalsIterator.map(i => i.end - i.start).sum

  def part2(input: String, range: Range): Long =
    val sensors = input.linesIterator.map(parseSensor).toSeq
    val (diet, row) = range.iterator.map(coverageTree(sensors, _)).zipWithIndex.find(!_._1(range)).get
    (diet.intervalsIterator.next._2 + 1) * 4000000L + row
