package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.graph.UnitGraph

import scala.collection.immutable.WrappedString

/** A 2d coordinate (or vector) with integer values. Integrates with [[Area]] and [[Region]],
  * and contains some integer specific extensions over [[Point]]. Extensions are
  * also provided for [[Seq]] and [[Map]] which represent grids.
  */
type Coord = Point[Int]
object Coord:
  val origin: Coord = Point.origin
  val unitUp: Coord = Point.unitUp
  val unitDown: Coord = Point.unitDown
  val unitLeft: Coord = Point.unitLeft
  val unitRight: Coord = Point.unitRight

  def apply(x: Int, y: Int): Coord = Point(x, y)

  /** Convert the typical advent input 2d grid into a map of coordinates to characters.
    * @param input the entire input file with newlines
    * @return a map of coordinates to characters
    */
  def gridToMap(input: String): Map[Coord, Char] = input.linesIterator
    .zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.map((c, x) => (Coord(x, y), c)))
    .toMap

extension (coord: Coord)
  def xRange(rhs: Coord): Range = coord.x.min(rhs.x) to coord.x.max(rhs.x)
  def yRange(rhs: Coord): Range = coord.y.min(rhs.y) to coord.y.max(rhs.y)
  def bounding(rhs: Coord): Area = Area(xRange = xRange(rhs), yRange = yRange(rhs))

extension [T](seq: Seq[Seq[T]])
  def apply(coord: Coord): T = seq(coord.row)(coord.col)

  def hasCoord(coord: Coord): Boolean =
    seq.indices.contains(coord.row) && seq(coord.row).indices.contains(coord.col)

extension (seq: Seq[String])(using String => WrappedString)
  def apply(coord: Coord): Char = seq(coord.row)(coord.col)

  def hasCoord(coord: Coord): Boolean =
    seq.indices.contains(coord.row) && seq(coord.row).indices.contains(coord.col)
