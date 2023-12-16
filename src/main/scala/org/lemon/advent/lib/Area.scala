package org.lemon.advent.lib

import Coord2._

case class Area(xRange: Range, yRange: Range) extends Iterable[Coord]:
  def iterator: Iterator[Coord] = for y <- yRange.iterator; x <- xRange.iterator yield (x, y)

object Area:

  extension (area: Area)
    inline def left = area.xRange.min
    inline def right = area.xRange.max
    inline def top = area.yRange.min
    inline def bottom = area.yRange.max

    inline def width = area.xRange.size
    inline def height = area.yRange.size

    inline def size = area.width * area.height

    inline def contains(coord: Coord): Boolean = area.xRange.contains(coord.x) && area.yRange.contains(coord.y)
    inline def apply(coord: Coord): Boolean = area.contains(coord)
