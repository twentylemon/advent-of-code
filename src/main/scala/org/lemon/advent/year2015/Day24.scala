package org.lemon.advent.year2015

private object Day24:

  def parse(input: String) = input.linesIterator.map(_.toInt).toIndexedSeq

  def subsetSum(set: Seq[Int], sum: Int, groups: Int): Iterator[Seq[Int]] =
    if groups == 1 then Option.when(set.sum == sum)(set).iterator
    else
      (1 to set.size).iterator
        .flatMap(set.combinations)
        .filter(_.sum == sum)
        .filter(p => subsetSum(set.diff(p), sum, groups - 1).nonEmpty)

  def part1(input: String) =
    val packages = parse(input)
    val sums = subsetSum(packages, packages.sum / 3, 3).buffered
    val min = sums.head.size
    sums.takeWhile(_.size == min).map(_.map(_.toLong).product).min

  def part2(input: String) =
    val packages = parse(input)
    val sums = subsetSum(packages, packages.sum / 4, 4).buffered
    val min = sums.head.size
    sums.takeWhile(_.size == min).map(_.map(_.toLong).product).min
