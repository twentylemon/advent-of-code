package org.lemon.advent.year2023

import org.lemon.advent.lib.pairs

import scala.collection.parallel.CollectionConverters._

private object Day24:

  case class Point(x: BigInt, y: BigInt, z: BigInt):
    def +(rhs: Point): Point = Point(x + rhs.x, y + rhs.y, z + rhs.z)
    def -(rhs: Point): Point = Point(x - rhs.x, y - rhs.y, z - rhs.z)
    def *(n: BigInt): Point = Point(x * n, y * n, z * n)

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

  def reframe(hail: Hail, velocity: Point): Hail = hail.copy(velocity = hail.velocity - velocity)
  def reframe(hail: Seq[Hail], velocity: Point): Seq[Hail] = hail.map(reframe(_, velocity))

  def round(intersection: (Double, Double)) =
    def round(x: Double) = BigDecimal(x).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
    (round(intersection._1), round(intersection._2))

  def chuck(stones: Seq[Hail]): Option[(BigInt, BigInt)] =
    val (first, second) = (stones.head, stones.tail.head)
    futureIntersection2d(first, second)
      .map(round)
      .flatMap(intersection =>
        val allAtPoint =
          stones.iterator.drop(2).forall(futureIntersection2d(first, _).map(round).contains(intersection))
        Option.when(allAtPoint)(intersection)
      )

  def t(hail: Hail, intersection: (BigInt, BigInt)) =
    val (x, y) = intersection
    if hail.velocity.x == 0 then (y - hail.position.y) / hail.velocity.y
    else (x - hail.position.x) / hail.velocity.x

  def z(stones: Seq[Hail], intersection: (BigInt, BigInt)): Option[BigInt] =
    def cross(lhs: Hail, rhs: Hail): BigInt =
      val tl = t(lhs, intersection)
      val tr = t(rhs, intersection)
      (lhs.position.z + tl * lhs.velocity.z - (rhs.position.z + tr * rhs.velocity.z)) / (tl - tr)

    val (first, second) = (stones.head, stones.tail.head)
    val at = cross(first, second)
    val allAtPoint = stones.iterator.drop(2).forall(cross(first, _) == at)
    Option.when(allAtPoint)(at)

  def part2(input: String, expand: Int = 1) =
    val hail = parse(input)
    val xRange = expand * hail.map(_.velocity.x).min to expand * hail.map(_.velocity.x).max
    val yRange = expand * hail.map(_.velocity.y).min to expand * hail.map(_.velocity.y).max

    val at = (for vx <- xRange; vy <- yRange yield Point(x = vx, y = vy, z = 0)).par
      .flatMap(velocity =>
        val stones = reframe(hail, velocity)
        chuck(stones)
          .flatMap(at => z(stones, at).map((at, _)))
          .map((at, z) => (at, velocity.copy(z = z)))
          .map((at, v) =>
            Point(
              x = at._1,
              y = at._2,
              z = t(stones.head, at) * (stones.head.velocity.z - v.z) + stones.head.position.z
            )
          )
      )
      .head
    at.x + at.y + at.z
