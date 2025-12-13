package org.lemon.advent.year2023

import org.lemon.advent.lib.*

import scala.collection.immutable.NumericRange
import scala.collection.parallel.CollectionConverters.*

private object Day05:

  type Range = NumericRange[Long]

  case class RangeMap(src: Range, dest: Range):
    def translate(src: Long) = dest.start + src - this.src.start

  case class Mapping(from: String, to: String, ranges: Seq[RangeMap])

  def parseMapping(block: String) =
    val lines = block.linesIterator.toSeq
    val (from, to) = lines.head match
      case s"$from-to-$to map:" => (from, to)

    val ranges = lines.tail
      .map(_.wsv.map(_.toLong))
      .map { case Seq(destStart, srcStart, length) =>
        RangeMap(srcStart until srcStart + length, destStart until destStart + length)
      }

    Mapping(from, to, ranges)

  def parse(input: String) =
    val chunks = input.chunks

    val seeds = chunks.head match
      case s"seeds: $nums" => nums.wsv.map(_.toLong)

    (seeds.toSeq, chunks.tail.map(parseMapping))

  def walk(seed: Long, mappings: Iterable[Mapping]): (Long, Long) =
    mappings.foldLeft((seed, Long.MaxValue)) {
      case ((almanacNum, safeToSkip), mapping) =>
        mapping.ranges
          .find(_.src.contains(almanacNum))
          .map(rng => (rng.translate(almanacNum), math.min(rng.src.end - almanacNum, safeToSkip)))
          .getOrElse((
            almanacNum,
            mapping.ranges
              .filter(_.src.start > almanacNum)
              .map(_.src.start - almanacNum)
              .minOption.getOrElse(safeToSkip)
          ))
    }

  def run(seeds: Seq[Range], mappings: Iterable[Mapping]) =
    Iterator.unfold((seeds, seeds.head.start))((seeds, currentSeed) =>
      if seeds.isEmpty then None
      else if seeds.head.contains(currentSeed) then
        val (loc, safeToSkip) = walk(currentSeed, mappings)
        Some((loc, (seeds, currentSeed + safeToSkip)))
      else if seeds.tail.isEmpty then None
      else Some((Long.MaxValue, (seeds.tail, seeds.tail.head.start)))
    )

  def part1(input: String) =
    val (seeds, mappings) = parse(input)
    run(seeds.map(s => s until s + 1), mappings).min

  def part2(input: String) =
    val (seeds, mappings) = parse(input)
    run(seeds.grouped(2).map { case Seq(start, length) => (start until start + length) }.toSeq, mappings).min
