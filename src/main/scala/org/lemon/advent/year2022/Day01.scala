package org.lemon.advent.year2022

private object Day01:

  def parse(input: String) = input.linesIterator.toSeq

  def groupByElf(input: Seq[String]): Seq[Seq[String]] = input match
    case Seq() => Seq.empty[Seq[String]]
    case seq => Seq(seq.takeWhile(!_.isBlank())) ++
        groupByElf(seq.dropWhile(!_.isBlank()).dropWhile(_.isBlank()))

  def parseCarry(elves: Seq[Seq[String]]): Seq[Int] = elves.map(_.map(_.toInt).sum)

  def part1(input: String) =
    val carry = (parse andThen groupByElf andThen parseCarry)(input)
    carry.max

  def part2(input: String) =
    val carry = (parse andThen groupByElf andThen parseCarry)(input)
    carry.sorted.takeRight(3).sum
