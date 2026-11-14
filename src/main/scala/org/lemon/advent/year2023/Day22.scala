package org.lemon.advent.year2023

import org.lemon.advent.lib.geometry.*

import scala.collection.mutable

private object Day22:

  type Brick = Cuboid[Int]

  def parse(input: String) = input.linesIterator.map {
    case s"$x1,$y1,$z1~$x2,$y2,$z2" => Cuboid(x1.toInt to x2.toInt, y1.toInt to y2.toInt, z1.toInt to z2.toInt)
  }.toVector

  def fall(brick: Brick, bricks: Seq[Brick]): Brick =
    val supportHeight = bricks
      .filter(below => brick.xy.overlaps(below.xy))
      .map(_.back + 1)
      .maxOption.getOrElse(1)
    brick.translate(Point3(0, 0, supportHeight - brick.front))

  def fall(bricks: Seq[Brick]): Seq[Brick] =
    bricks.foldLeft(Vector.empty[Brick])((stable, brick) => stable :+ fall(brick, stable))

  def supports(lhs: Brick, rhs: Brick) =
    lhs.front - 1 == rhs.back && lhs.xy.overlaps(rhs.xy)

  def supporting(bricks: Seq[Brick]) =
    bricks.map(brick => (brick, bricks.filter(supports(_, brick)))).toMap

  def supportedBy(bricks: Seq[Brick]) =
    bricks.map(brick => (brick, bricks.filter(supports(brick, _)))).toMap

  def part1(input: String) =
    val bricks = parse(input).sortBy(_.front)
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

    while queue.nonEmpty do
      val next = queue.dequeue()
      falling += next
      queue ++= supports(next).filter(supported(_).forall(falling))

    falling.size - 1

  def part2(input: String) =
    val bricks = parse(input).sortBy(_.front)
    val stable = fall(bricks)
    val supports = supporting(stable)
    val supported = supportedBy(stable)
    stable
      .map(totalDestruction(_, stable, supports, supported))
      .sum
