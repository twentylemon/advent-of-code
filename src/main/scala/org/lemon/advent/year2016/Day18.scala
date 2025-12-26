package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day18:

  def trapped(previous: String)(pos: Int) =
    previous.lift(pos - 1).getOrElse('.') != previous.lift(pos + 1).getOrElse('.')

  def buildRoom(initial: String) =
    Iterator.iterate(initial)(row =>
      row.indices.map {
        case i if trapped(row)(i) => '^'
        case _ => '.'
      }.mkString
    )

  def part1(input: String, rows: Int = 40) =
    buildRoom(input.trim).take(rows).map(_.count(_ == '.')).sum

  def part2(input: String) =
    part1(input, rows = 400000)
