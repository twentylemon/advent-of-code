package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.`2d`.Coord._

import scala.collection.mutable

private object Day18:

  case class Edge(direction: Direction, length: Int)

  def parse(input: String) = input.linesIterator
    .map(_ match
      case s"$dir $len ($_)" => Edge(direction = Direction(dir), length = len.toInt)
    )
    .toSeq

  def parseHex(input: String) = input.linesIterator
    .map(_ match
      case s"$_ $_ (#${len}0)" => Edge(direction = Direction.Right, length = java.lang.Integer.parseInt(len, 16))
      case s"$_ $_ (#${len}1)" => Edge(direction = Direction.Down, length = java.lang.Integer.parseInt(len, 16))
      case s"$_ $_ (#${len}2)" => Edge(direction = Direction.Left, length = java.lang.Integer.parseInt(len, 16))
      case s"$_ $_ (#${len}3)" => Edge(direction = Direction.Up, length = java.lang.Integer.parseInt(len, 16))
    )
    .toSeq

  def trace(edges: Seq[Edge]) = edges
    .foldLeft(List(origin))((list, edge) => list.head.shift(edge.direction, edge.length) :: list)

  def innerArea(corners: Seq[Coord]) =
    corners.zip(corners.tail)
      .map((lhs, rhs) => lhs.x.toLong * rhs.y - lhs.y.toLong * rhs.x)
      .sum
      .abs / 2

  def solve(prs: String => Seq[Edge])(input: String) =
    val edges = prs(input)
    val corners = trace(edges)
    innerArea(corners) + (edges.map(_.length).sum / 2 + 1)

  def part1 = solve(parse)
  
  def part2 = solve(parseHex)
