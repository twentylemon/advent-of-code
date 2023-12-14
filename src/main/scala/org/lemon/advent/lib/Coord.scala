package org.lemon.advent.lib

object Coord2:

  type Coord = (Int, Int)

  extension (coord: Coord)
    inline def x = coord._1
    inline def y = coord._2
    inline def row = coord._2
    inline def col = coord._1
    inline def up = (coord.x, coord.y - 1)
    inline def down = (coord.x, coord.y + 1)
    inline def left = (coord.x - 1, coord.y)
    inline def right = (coord.x + 1, coord.y)

    inline def shiftUp(n: Int) = (coord.x, coord.y - n)
    inline def shiftDown(n: Int) = (coord.x, coord.y + n)
    inline def shiftLeft(n: Int) = (coord.x - n, coord.y)
    inline def shiftRight(n: Int) = (coord.x + n, coord.y)

    def adjacent = Seq(coord.up, coord.down, coord.left, coord.right)

    def surrounding = Seq(
      coord.up,
      coord.down,
      coord.left,
      coord.right,
      coord.up.left,
      coord.up.right,
      coord.down.left,
      coord.down.right
    )

    def manhattan(rhs: Coord) = (coord.x - rhs.x).abs + (coord.y - rhs.y).abs

    def xRange(rhs: Coord) = math.min(coord.x, rhs.x) until math.max(coord.x, rhs.x)
    def yRange(rhs: Coord) = math.min(coord.y, rhs.y) until math.max(coord.y, rhs.y)

    inline def +(rhs: Coord) = (coord.x + rhs.x, coord.y + rhs.y)
    inline def -(rhs: Coord) = (coord.x - rhs.x, coord.y - rhs.y)
