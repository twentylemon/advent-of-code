package org.lemon.advent.year2023

import org.lemon.advent.lib.pairs

private object Day24:

  case class Point(x: BigInt, y: BigInt, z: BigInt):
    def +(rhs: Point): Point = Point(x + rhs.x, y + rhs.y, z + rhs.z)

  case class Hail(position: Point, velocity: Point)

  def parse(input: String) = input.linesIterator
    .map(_ match
      case s"$x, $y, $z @ $vx, $vy, $vz" => Hail(
          position = Point(x = x.trim.toLong, y = y.trim.toLong, z = z.trim.toLong),
          velocity = Point(x = vx.trim.toLong, y = vy.trim.toLong, z = vz.trim.toLong)
        )
    )
    .toSeq

  def futureIntersection2d(lhs: Hail, rhs: Hail): Option[(Double, Double)] =
    val Point(x1, y1, _) = lhs.position
    val Point(x2, y2, _) = lhs.position + lhs.velocity
    val Point(x3, y3, _) = rhs.position
    val Point(x4, y4, _) = rhs.position + rhs.velocity

    val denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    if denominator == 0 then None
    else
      val x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)).doubleValue / denominator.toDouble
      val y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)).doubleValue / denominator.toDouble
      Option.when(
        lhs.velocity.x.sign == (x - x1.doubleValue).sign && lhs.velocity.y.sign == (y - y1.doubleValue).sign &&
          rhs.velocity.x.sign == (x - x3.doubleValue).sign && rhs.velocity.y.sign == (y - y3.doubleValue).sign
      )((x, y))

  def part1(input: String, min: Double = 200000000000000.0, max: Double = 400000000000000.0) =
    val hail = parse(input)
    hail.pairs
      .flatMap(futureIntersection2d)
      .filter((x, y) => x >= min && x <= max && y >= min && y <= max)
      .size

  def part2(input: String) = 0
