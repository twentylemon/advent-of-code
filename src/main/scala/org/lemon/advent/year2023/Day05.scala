package org.lemon.advent.year2023

import scala.collection.immutable.NumericRange

private object Day05:

  case class RangeMap(destStart: Long, srcRange: NumericRange[Long])

  case class Mapping(from: String, to: String, ranges: Seq[(NumericRange[Long], NumericRange[Long])]):
    def translate(src: Long) = ranges
      .find((s, _) => s.contains(src))
      .map((s, d) => d.start + src - s.start)
      .getOrElse(src)

  def parseMapping(block: String) =
    val lines = block.split("\n")
    val (from, to) = lines.head match
      case s"$from-to-$to map:" => (from, to)

    val ranges = lines.tail
      .map(_.split(" "))
      .map(_.map(x => x.toLong))
      .map { case Array(destStart, srcStart, length) =>
        (srcStart until srcStart + length, destStart until destStart + length)
      }

    Mapping(from, to, ranges)

  def parse(input: String) =
    val chunks = input.split("\n\n")

    val seeds = chunks.head match
      case s"seeds: $nums" => nums.split(" ").map(_.trim).map(_.toLong)

    (seeds.toSeq, chunks.tail.map(parseMapping))

  def walk(seed: Long, mappings: Iterable[Mapping]) =
    mappings.foldLeft(seed)((id, mapping) => mapping.translate(id))

  def part1(input: String) =
    val (seeds, mappings) = parse(input)
    seeds
      .map(walk(_, mappings))
      .min

  def part2(input: String) =
    val (seeds, mappings) = parse(input)
    // val min = seeds
    //   .grouped(2)
    //   .flatMap { case Seq(start, length) => (start until start + length) }
    //   .minBy(walk(_, mappings))
    // walk(min, mappings)
    0
