package org.lemon.advent.year2024

import org.lemon.advent.lib.*

private object Day11:

  def parse(input: String) = input.wsv.map(_.toLong)

  def blink(n: Long): Seq[Long] =
    if n == 0 then Seq(1)
    else
      val str = n.toString
      if str.size % 2 == 0 then Seq(str.take(str.size / 2).toLong, str.drop(str.size / 2).toLong)
      else Seq(n * 2024L)

  def process(stones: Map[Long, Long]) =
    Iterator.iterate(stones)(_
      .map((stone, count) => blink(stone).groupMapReduce(identity)(_ => count)(_ + _))
      .reduce((a, b) => a ++ b.map { case (k, v) => k -> (v + a.getOrElse(k, 0L)) }))

  def part1(input: String) =
    val stones = parse(input).groupMapReduce(identity)(_ => 1L)(_ + _)
    process(stones).nth(25).values.sum

  def part2(input: String) =
    val stones = parse(input).groupMapReduce(identity)(_ => 1L)(_ + _)
    process(stones).nth(75).values.sum
