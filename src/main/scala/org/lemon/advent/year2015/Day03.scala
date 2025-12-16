package org.lemon.advent.year2015

import org.lemon.advent.lib.`2d`.*

private object Day03:

  def parse(input: String) = input.map(Direction(_))

  def part1(input: String) =
    val steps = parse(input)
    steps.scanLeft(Coord.origin)(_ + _).distinct.size

  def part2(input: String) =
    val steps = parse(input)
    val santa = steps.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val robosanta = steps.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
    (santa.scanLeft(Coord.origin)(_ + _) ++ robosanta.scanLeft(Coord.origin)(_ + _)).distinct.size
