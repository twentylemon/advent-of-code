package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day06:

  def parse(input: String) = input.linesIterator.toSeq

  def part1(input: String) =
    parse(input).transpose
      .map(_.frequencies.maxBy(_._2)._1)
      .mkString

  def part2(input: String) =
    parse(input).transpose
      .map(_.frequencies.minBy(_._2)._1)
      .mkString
