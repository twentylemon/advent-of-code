package org.lemon.advent.year2022

private object Day03:

  def priority(ch: Char) = if ch.isUpper then ch.toInt - 'A'.toInt + 27 else ch.toInt - 'a'.toInt + 1

  def compartmentsOf(sack: String): Seq[Seq[Char]] =
    sack
      .splitAt(sack.length / 2)
      .toList
      .map(_.toCharArray.toSeq)

  def getPriorityOfCommon(compartments: Seq[Seq[Char]]) = compartments
    .map(_.toSet)
    .reduce(_ intersect _)
    .map(priority)
    .sum

  def part1(input: String) = input.linesIterator.map(compartmentsOf).map(getPriorityOfCommon).sum

  def part2(input: String) = input.linesIterator
    .map(_.toCharArray.toSeq)
    .grouped(3)
    .map(getPriorityOfCommon)
    .sum
