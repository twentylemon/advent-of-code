package org.lemon.advent.year2024

import org.lemon.advent.lib.*

import scala.collection.mutable

private object Day19:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.*
    input match
      case Chunk(towels, targets) => (towels.csv, targets.linesIterator.toSeq)

  def countLayouts(towels: Seq[String], target: String) =
    lazy val count: String => Long = memoize {
      case "" => 1L
      case remaining =>
        towels
          .filter(remaining.startsWith)
          .map(towel => count(remaining.drop(towel.length)))
          .sum
    }
    count(target)

  def part1(input: String) =
    val (towels, targets) = parse(input)
    targets.map(countLayouts(towels, _)).count(_ > 0)

  def part2(input: String) =
    val (towels, targets) = parse(input)
    targets.map(countLayouts(towels, _)).sum
