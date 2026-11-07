package org.lemon.advent.year2022

import org.lemon.advent.lib.*

private object Day01:

  def parse(input: String) = input.chunks.map(_.linesIterator.toSeq)

  def parseCarry(elves: Seq[Seq[String]]): Seq[Int] = elves.map(_.map(_.toInt).sum)

  def part1(input: String) =
    val carry = (parse andThen parseCarry)(input)
    carry.max

  def part2(input: String) =
    val carry = (parse andThen parseCarry)(input)
    carry.sorted.takeRight(3).sum
