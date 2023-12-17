package org.lemon.advent.lib

import Coord2._

object Area:

  def apply(grid: Map[Coord, _]): Area = apply(grid.keySet)

  def apply(coords: Iterable[Coord]): Area =
    val xs = coords.map(_.x)
    val ys = coords.map(_.y)
    Area(xRange = xs.min to xs.max, yRange = ys.min to ys.max)

  def apply(grid: Seq[Seq[_]]): Area = Area(yRange = grid.indices, xRange = grid.head.indices)

  given Conversion[Area, Iterator[Coord]] with
    def apply(area: Area): Iterator[Coord] = for y <- area.yRange.iterator; x <- area.xRange.iterator yield (x, y)

case class Area(xRange: Range, yRange: Range):
  def left = xRange.min
  def right = xRange.max
  def top = yRange.min
  def bottom = yRange.max

  def topLeft: Coord = (left, top)
  def bottomRight: Coord = (right, bottom)
  def topRight: Coord = (right, top)
  def bottomLeft: Coord = (left, bottom)

  def width = xRange.size
  def height = yRange.size

  def size = width * height

  def contains(coord: Coord): Boolean = xRange.contains(coord.x) && yRange.contains(coord.y)
  def apply(coord: Coord): Boolean = contains(coord)

  def col(x: Int): Iterator[Coord] =
    assert(xRange.contains(x), s"$this  does not contain column  $x")
    for y <- yRange.iterator yield (x, y)

  def leftCol: Iterator[Coord] = col(left)
  def rightCol: Iterator[Coord] = col(right)

  def row(y: Int): Iterator[Coord] =
    assert(yRange.contains(y), s"$this  does not contain row  $y")
    for x <- xRange.iterator yield (x, y)

  def topRow: Iterator[Coord] = row(top)
  def bottomRow: Iterator[Coord] = row(bottom)
