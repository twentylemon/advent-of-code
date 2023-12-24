package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.graph.UnitGraph

object Coord:

  given Conversion[(Int, Int), Coord] with
    def apply(coord: (Int, Int)): Coord = Coord(x = coord._1, y = coord._2)

  given Ordering[Coord] = Ordering.by[Coord, Int](_.x).orElseBy(_.y)

  val origin: Coord = (0, 0)
  val unitUp: Coord = (0, -1)
  val unitDown: Coord = (0, 1)
  val unitLeft: Coord = (-1, 0)
  val unitRight: Coord = (1, 0)

  extension [T](seq: Seq[Seq[T]])
    def apply(coord: Coord): T = seq(coord.row)(coord.col)

    def hasCoord(coord: Coord): Boolean =
      seq.indices.contains(coord.row) && seq(coord.row).indices.contains(coord.col)
  
  extension [V](grid: Map[Coord, V])
    def toGridAdjacencyList: UnitGraph[Coord] =
      grid.map((coord, _) => (coord, coord.adjacent.filter(grid.contains)))

  def gridToMap(input: String): Map[Coord, Char] = input.linesIterator
    .zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.map((c, x) => (Coord(x, y), c)))
    .toMap

case class Coord(x: Int, y: Int):
  def row = y
  def col = x

  def up: Coord = copy(y = y - 1)
  def down: Coord = copy(y = y + 1)
  def left: Coord = copy(x = x - 1)
  def right: Coord = copy(x = x + 1)
  def move(direction: Direction): Coord = this + direction.unitVector

  def shiftUp(n: Int): Coord = copy(y = y - n)
  def shiftDown(n: Int): Coord = copy(y = y + n)
  def shiftLeft(n: Int): Coord = copy(x = x - n)
  def shiftRight(n: Int): Coord = copy(x = x + n)
  def shift(direction: Direction, n: Int): Coord = this + (direction.unitVector * n)

  def adjacent: Seq[Coord] = Seq(up, down, left, right)
  def surrounding: Seq[Coord] = Seq(up, down, left, right, up.left, up.right, down.left, down.right)

  def manhattan(rhs: Coord) = (x - rhs.x).abs + (y - rhs.y).abs

  def xRange(rhs: Coord): Range = math.min(x, rhs.x) to math.max(x, rhs.x)
  def yRange(rhs: Coord): Range = math.min(y, rhs.y) to math.max(y, rhs.y)
  def bounding(rhs: Coord): Area = Area(xRange = xRange(rhs), yRange = yRange(rhs))

  def flip: Coord = Coord(x = y, y = x)

  def +(rhs: Coord): Coord = (x + rhs.x, y + rhs.y)
  def +(direction: Direction): Coord = move(direction)
  def -(rhs: Coord): Coord = (x - rhs.x, y - rhs.y)
  def *(n: Int): Coord = (x * n, y * n)
