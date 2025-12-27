package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day19:

  def part1(input: String) =
    val n = input.trim.toInt
    val powerOf2 = Iterator.iterate(1)(_ * 2).takeWhile(_ <= n).last
    2 * (n - powerOf2) + 1

  def part2(input: String) =
    val n = input.trim.toInt
    val powerOf3 = Iterator.iterate(1)(_ * 3).takeWhile(_ <= n).last
    if n == powerOf3 then n
    else if n <= 2 * powerOf3 then n - powerOf3
    else 2 * (n - 2 * powerOf3) + powerOf3
