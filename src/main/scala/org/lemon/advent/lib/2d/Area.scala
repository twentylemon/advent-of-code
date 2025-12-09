package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.*
import scala.collection.immutable.WrappedString

/** A 2d rectangular area with integer bounds. Integrates with [[Coord]] and [[Interval]].
  */
type Area = Rect[Int]

object Area:
  def apply(grid: Map[Coord, ?]): Area = Rect(grid)
  def apply(coords: Iterable[Coord]): Area = Rect(coords)
  def apply(grid: Seq[Seq[?]]): Area = Rect(grid)
  def apply(lines: Seq[String])(using String => WrappedString): Area = Rect(lines)
  def apply(xRange: Range, yRange: Range): Area = Rect(xRange.toInterval, yRange.toInterval)
  def apply(xRange: Interval[Int], yRange: Interval[Int]): Area = Rect(xRange, yRange)
  def apply(xRange: Range, yRange: Interval[Int]): Area = Rect(xRange.toInterval, yRange)
  def apply(xRange: Interval[Int], yRange: Range): Area = Rect(xRange, yRange.toInterval)
  def apply(left: Int, right: Int, top: Int, bottom: Int): Area = Rect(left, right, top, bottom)
  def apply(width: Int, height: Int): Area = Rect(width, height)

  extension [T](seq: Seq[String])(using String => WrappedString)
    def apply(area: Area): Seq[Seq[Char]] =
      seq.slice(area.top, area.bottom + 1).map(_.slice(area.left, area.right + 1))

  extension [T](seq: Seq[Seq[T]])
    def apply(area: Area): Seq[Seq[T]] =
      seq.slice(area.top, area.bottom + 1).map(_.slice(area.left, area.right + 1))
