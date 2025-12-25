package org.lemon.advent.year2015

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import scala.collection.Searching.*

private object Day06:

  sealed trait Op
  case object TurnOff extends Op
  case object TurnOn extends Op
  case object Toggle extends Op

  given Ordering[Area] = Ordering.by(a => (a.left, a.top))

  def parse(input: String) =
    def area(x1: String, x2: String, y1: String, y2: String) = Area(x1.toInt, x2.toInt, y1.toInt, y2.toInt)
    input.linesIterator.map {
      case s"turn off $x1,$y1 through $x2,$y2" => (TurnOff, area(x1, x2, y1, y2))
      case s"turn on $x1,$y1 through $x2,$y2" => (TurnOn, area(x1, x2, y1, y2))
      case s"toggle $x1,$y1 through $x2,$y2" => (Toggle, area(x1, x2, y1, y2))
    }.toSeq

  def compress(areas: Seq[Area]) =
    val xs = areas.flatMap(area => Seq(area.left, area.right + 1)).distinct.sorted
    val ys = areas.flatMap(area => Seq(area.top, area.bottom + 1)).distinct.sorted
    xs.sliding2
      .cartesianProduct(ys.sliding2)
      .map { case ((x1, x2), (y1, y2)) => Area(x1 until x2, y1 until y2) }
      .filter(_.nonEmpty)
      .toIndexedSeq
      .sorted

  def affected(cells: Seq[Area], area: Area) =
    val lo = cells.search(area).insertionPoint
    val hi = cells.search(Area(area.right + 1, area.right + 1, 0, 0)).insertionPoint
    cells.slice(lo, hi).filter(_.intersects(area))

  def part1(input: String) =
    val ops = parse(input)
    val cells = compress(ops.map(_._2))
    ops.foldLeft(Set.empty[Area]) { case (acc, (op, area)) =>
      val affects = affected(cells, area).toSet
      op match
        case TurnOn => acc ++ affects
        case TurnOff => acc -- affects
        case Toggle => (acc -- affects) ++ (affects -- acc)
    }.view
      .map(_.size)
      .sum

  def part2(input: String) =
    val ops = parse(input)
    val cells = compress(ops.map(_._2))
    val init = cells.map(_ -> 0).toMap
    def brightness(op: Op) =
      op match
        case TurnOn => 1
        case TurnOff => -1
        case Toggle => 2

    ops.foldLeft(init) { case (acc, (op, area)) =>
      affected(cells, area).foldLeft(acc)((acc, cell) =>
        acc.updatedWith(cell)(_.map(b => (b + brightness(op)) `max` 0))
      )
    }
      .map((cell, bright) => bright * cell.size)
      .sum
