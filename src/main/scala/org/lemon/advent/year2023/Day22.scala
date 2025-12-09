package org.lemon.advent.year2023

import org.lemon.advent.lib.Interval

import scala.collection.mutable

private object Day22:

  case class Brick(x: Interval[Int], y: Interval[Int], z: Interval[Int])

  def parse(input: String) = input.linesIterator
    .map(_ match
      case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
        Brick(Interval(x1.toInt to x2.toInt), Interval(y1.toInt to y2.toInt), Interval(z1.toInt to z2.toInt))
    )
    .toVector

  def fall(brick: Brick, bricks: Seq[Brick]): Brick =
    val supportHeight = bricks
      .filter(below => brick.x.intersects(below.x))
      .filter(below => brick.y.intersects(below.y))
      .map(_.z.max + 1)
      .maxOption.getOrElse(1)
    brick.copy(z = Interval(supportHeight to supportHeight + brick.z.size - 1))

  def fall(bricks: Seq[Brick]): Seq[Brick] =
    bricks.foldLeft(Vector.empty[Brick])((stable, brick) => stable :+ fall(brick, stable))

  def supports(lhs: Brick, rhs: Brick) =
    lhs.z.min - 1 == rhs.z.max && lhs.x.intersects(rhs.x) && lhs.y.intersects(rhs.y)

  def supporting(bricks: Seq[Brick]) =
    bricks.map(brick => (brick, bricks.filter(supports(_, brick)))).toMap

  def supportedBy(bricks: Seq[Brick]) =
    bricks.map(brick => (brick, bricks.filter(supports(brick, _)))).toMap

  def part1(input: String) =
    val bricks = parse(input).sortBy(_.z.min)
    val stable = fall(bricks)
    val supports = supporting(stable)
    val supported = supportedBy(stable)
    supports.count((brick, support) => support.forall(x => supported(x).exists(_ != brick)))

  def totalDestruction(
      brick: Brick,
      bricks: Seq[Brick],
      supports: Map[Brick, Seq[Brick]],
      supported: Map[Brick, Seq[Brick]]
  ) =
    val falling = mutable.Set.empty[Brick]
    val queue = mutable.Queue(brick)

    while !queue.isEmpty do
      val next = queue.dequeue
      falling += next
      queue ++= supports(next).filter(supported(_).forall(falling))

    falling.size - 1

  def part2(input: String) =
    val bricks = parse(input).sortBy(_.z.min)
    val stable = fall(bricks)
    val supports = supporting(stable)
    val supported = supportedBy(stable)
    stable
      .map(totalDestruction(_, stable, supports, supported))
      .sum
