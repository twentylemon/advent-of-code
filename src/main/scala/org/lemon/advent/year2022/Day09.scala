package org.lemon.advent.year2022

import org.lemon.advent.lib.`2d`.*

import scala.collection.mutable

private object Day09:

  type Rope = Seq[Coord]

  def drag(head: Coord, tail: Coord): Coord =
    if head.chessboard(tail) > 1 then
      val diff = head - tail
      tail + (diff.x.sign, diff.y.sign)
    else tail

  def update(rope: Rope, step: (Coord => Coord), n: Int, tailPos: mutable.Map[Coord, Int]): Rope =
    val newRope = mutable.Seq(rope*)
    for _ <- 1 to n do
      newRope(0) = step(newRope(0))
      for i <- 1 to rope.size - 1 do newRope(i) = drag(newRope(i - 1), newRope(i))
      tailPos(newRope.last) += 1
    newRope.toSeq

  def move(move: String, rope: Rope, tailPos: mutable.Map[Coord, Int]) = move match
    case s"L $n" => update(rope, _.left, n.toInt, tailPos)
    case s"R $n" => update(rope, _.right, n.toInt, tailPos)
    case s"U $n" => update(rope, _.up, n.toInt, tailPos)
    case s"D $n" => update(rope, _.down, n.toInt, tailPos)

  def trackTail(in: String, numberOfKnots: Int) =
    val tailPos = mutable.Map.empty[Coord, Int].withDefaultValue(0)
    var rope = Seq.fill(numberOfKnots)(Coord.origin)
    in.linesIterator.foldLeft(rope)((r, m) => move(m, r, tailPos))
    tailPos

  def part1(input: String) = trackTail(input, 2).size

  def part2(input: String) = trackTail(input, 10).size
