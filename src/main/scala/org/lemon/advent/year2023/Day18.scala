package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.`2d`.Coord._

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

  def innerArea(corners: Seq[Coord]) =
    corners.zip(corners.tail)
      .map((lhs, rhs) => lhs.x.toLong * rhs.y - lhs.y.toLong * rhs.x)
      .sum
      .abs / 2

  def solve(prs: String => Seq[Coord])(input: String) =
    val edges = prs(input)
    val corners = trace(edges)
    innerArea(corners) + (edges.map(_ manhattan origin).sum / 2 + 1)

  def part1 = solve(parse)

  def part2 = solve(parseHex)
