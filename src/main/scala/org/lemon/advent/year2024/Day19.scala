package org.lemon.advent.year2024

import org.lemon.advent.lib._

import scala.collection.mutable

private object Day19:

  def parse(input: String) = input match
    case Chunk(towels, targets) => (towels.split(",").map(_.trim).toSeq, targets.linesIterator.toSeq)

  def countLayouts(towels: Seq[String], target: String) =
    val memo = mutable.Map("" -> 1L)
    def count(remaining: String, built: Seq[String]): Long =
      memo.get(remaining) match
        case Some(x) => x
        case None =>
          val result = towels
            .filter(remaining.startsWith)
            .map(towel => count(remaining.drop(towel.length), built :+ towel))
            .sum
          memo(remaining) = result
          result
    count(target, Seq())

  def part1(input: String) =
    val (towels, targets) = parse(input)
    targets.map(countLayouts(towels, _)).count(_ > 0)

  def part2(input: String) =
    val (towels, targets) = parse(input)
    targets.map(countLayouts(towels, _)).sum
