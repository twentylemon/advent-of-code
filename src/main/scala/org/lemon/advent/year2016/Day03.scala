package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day03:

  def parse(input: String) = input.linesIterator.map(_.wsv.map(_.toInt)).toSeq

  def part1(input: String) =
    parse(input).map(_.sorted).count(t => t(0) + t(1) > t(2))

  def part2(input: String) =
    parse(input).transpose.flatten.grouped(3).map(_.sorted).count(t => t(0) + t(1) > t(2))
