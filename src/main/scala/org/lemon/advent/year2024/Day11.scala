package org.lemon.advent.year2024

private object Day11:

  def parse(input: String) = input.split("\\s+").map(_.toLong).toSeq

  def blink(n: Long): Seq[Long] =
    if n == 0 then Seq(1)
    else
      val str = n.toString
      if str.size % 2 == 0 then Seq(str.take(str.size / 2).toLong, str.drop(str.size / 2).toLong)
      else Seq(n * 2024L)

  def part1(input: String) =
    val stones = parse(input)
    Iterator.iterate(stones)(_.flatMap(blink)).drop(25).next.size

  def part2(input: String) =
    val stones = parse(input)
    // it's too chunky
    // Iterator.iterate(stones)(_.flatMap(blink)).drop(75).next.size
    0
