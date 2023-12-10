package org.lemon.advent

object Coord2:

  type Coord = (Int, Int)

  extension (coord: Coord)
    def x = coord._1
    def y = coord._2
    def row = coord._2
    def col = coord._1
    def up = (coord.x, coord.y - 1)
    def down = (coord.x, coord.y + 1)
    def left = (coord.x - 1, coord.y)
    def right = (coord.x + 1, coord.y)

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
