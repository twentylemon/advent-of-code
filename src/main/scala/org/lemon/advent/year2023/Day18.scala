package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.`2d`.Coord.*

import scala.collection.mutable

private object Day18:

  def parse(input: String) = input.linesIterator
    .map(_ match
      case s"$dir $len ($_)" => Direction(dir).unitVector * len.toInt
    )
    .toSeq

  def parseHex(input: String) = input.linesIterator
    .map(_ match
      case s"$_ $_ (#${len}0)" => Direction.Right.unitVector * java.lang.Integer.parseInt(len, 16)
      case s"$_ $_ (#${len}1)" => Direction.Down.unitVector * java.lang.Integer.parseInt(len, 16)
      case s"$_ $_ (#${len}2)" => Direction.Left.unitVector * java.lang.Integer.parseInt(len, 16)
      case s"$_ $_ (#${len}3)" => Direction.Up.unitVector * java.lang.Integer.parseInt(len, 16)
    )
    .toSeq

  def trace(edges: Seq[Coord]) = edges
    .foldLeft(List(origin))((list, edge) => list.head + edge :: list)

  def solve(prs: String => Seq[Coord])(input: String) =
    val edges = prs(input)
    val corners = trace(edges).map(_.to[Long])
    corners.shoelaceArea + (edges.map(_ `manhattan` origin).sum / 2 + 1)

  def part1 = solve(parse)

  def part2 = solve(parseHex)
