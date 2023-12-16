package org.lemon.advent.lib

object Coord2:

  given Conversion[(Int, Int), Coord] with
    def apply(coord: (Int, Int)): Coord = Coord(x = coord._1, y = coord._2)

  case class Coord(x: Int, y: Int):
    inline def row = y
    inline def col = x
    inline def up: Coord = copy(y = y - 1)
    inline def down: Coord = copy(y = y + 1)
    inline def left: Coord = copy(x = x - 1)
    inline def right: Coord = copy(x = x + 1)

    inline def shiftUp(n: Int): Coord = copy(y = y - n)
    inline def shiftDown(n: Int): Coord = copy(y = y + n)
    inline def shiftLeft(n: Int): Coord = copy(x = x - n)
    inline def shiftRight(n: Int): Coord = copy(x = x + n)

    def adjacent: Seq[Coord] = Seq(up, down, left, right)

    def surrounding: Seq[Coord] = Seq(up, down, left, right, up.left, up.right, down.left, down.right)

    def manhattan(rhs: Coord) = (x - rhs.x).abs + (y - rhs.y).abs

    def xRange(rhs: Coord): Range = math.min(x, rhs.x) until math.max(x, rhs.x)
    def yRange(rhs: Coord): Range = math.min(y, rhs.y) until math.max(y, rhs.y)

    inline def +(rhs: Coord): Coord = (x + rhs.x, y + rhs.y)
    inline def -(rhs: Coord): Coord = (x - rhs.x, y - rhs.y)

  val unitUp: Coord = (0, -1)
  val unitDown: Coord = (0, 1)
  val unitLeft: Coord = (-1, 0)
  val unitRight: Coord = (1, 0)

  extension [T](seq: Seq[Seq[T]])
    def apply(coord: Coord): T = seq(coord.y)(coord.x)
