package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.graph.UnitGraph
import org.lemon.advent.lib._

import scala.collection.immutable.WrappedString
import scala.math.Integral.Implicits._
import scala.math.Ordering.Implicits._

private def `0`[T: Integral]: T = Integral[T].zero
private def `1`[T: Integral]: T = Integral[T].one

object Point:
  given [T: Integral]: Conversion[(T, T), Point[T]] = (coord: (T, T)) => Point(x = coord._1, y = coord._2)

  given [T: Integral]: Ordering[Point[T]] = Ordering.by[Point[T], T](_.x).orElseBy(_.y)

  def origin[T: Integral]: Point[T] = Point(Integral[T].zero, Integral[T].zero)
  def unitUp[T: Integral]: Point[T] = Point(Integral[T].zero, -Integral[T].one)
  def unitDown[T: Integral]: Point[T] = Point(Integral[T].zero, Integral[T].one)
  def unitLeft[T: Integral]: Point[T] = Point(-Integral[T].one, Integral[T].zero)
  def unitRight[T: Integral]: Point[T] = Point(Integral[T].one, Integral[T].zero)

  def apply[T: Integral](x: Int, y: Int): Point[T] = Point(Integral[T].fromInt(x), Integral[T].fromInt(y))
  def apply[T: Integral](coord: Coord): Point[T] = apply(coord.x, coord.y)

case class Point[N: Integral](x: N, y: N):
  def row = y
  def col = x

  def up: Point[N] = copy(y = y - `1`)
  def down: Point[N] = copy(y = y + `1`)
  def left: Point[N] = copy(x = x - `1`)
  def right: Point[N] = copy(x = x + `1`)
  def move(direction: Direction): Point[N] = this + Point[N](direction.unitVector)

  def shiftUp(n: N): Point[N] = copy(y = y - n)
  def shiftDown(n: N): Point[N] = copy(y = y + n)
  def shiftLeft(n: N): Point[N] = copy(x = x - n)
  def shiftRight(n: N): Point[N] = copy(x = x + n)
  def shift(direction: Direction, n: N): Point[N] = this + (Point[N](direction.unitVector) * n)

  def adjacent: Seq[Point[N]] = Seq(up, down, left, right)
  def surrounding: Seq[Point[N]] = Seq(up, down, left, right, up.left, up.right, down.left, down.right)

  def manhattan(rhs: Point[N]): N = (x - rhs.x).abs + (y - rhs.y).abs

  def xRange(rhs: Point[N]): Range = x.min(rhs.x).toInt to x.max(rhs.x).toInt
  def yRange(rhs: Point[N]): Range = y.min(rhs.y).toInt to y.max(rhs.y).toInt
  def bounding(rhs: Point[N]): Area = Area(xRange = xRange(rhs), yRange = yRange(rhs))

  def flip = Point[N](x = y, y = x)

  def +(rhs: Point[N]): Point[N] = (x + rhs.x, y + rhs.y)
  def +(direction: Direction): Point[N] = move(direction)
  def -(rhs: Point[N]): Point[N] = (x - rhs.x, y - rhs.y)
  def -(direction: Direction): Point[N] = move(direction.turnAround)
  def *(n: N): Point[N] = (x * n, y * n)

  def directionTo(rhs: Point[N]): Option[Direction] =
    val Point[N](dx, dy) = rhs - this
    if dx == `0` && dy < `0` then Some(Direction.Up)
    else if dx == `0` && dy > `0` then Some(Direction.Down)
    else if dx < `0` && dy == `0` then Some(Direction.Left)
    else if dx > `0` && dy == `0` then Some(Direction.Right)
    else None

  def asVec: VecT[N] = VecT[N](this)

type Coord = Point[Int]
object Coord:
  val origin: Coord = (0, 0)
  val unitUp: Coord = (0, -1)
  val unitDown: Coord = (0, 1)
  val unitLeft: Coord = (-1, 0)
  val unitRight: Coord = (1, 0)

  def apply(x: Int, y: Int): Coord = Point(x, y)

  def gridToMap(input: String): Map[Coord, Char] = input.linesIterator
    .zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.map((c, x) => (Coord(x, y), c)))
    .toMap

opaque type VecT[N] = Point[N]
type Vec = VecT[Int]
object VecT:
  def apply[N: Integral](coord: Point[N]): VecT[N] = coord
  def apply[N: Integral](x: N, y: N): VecT[N] = Point[N](x, y)

  given [N: Integral]: Conversion[VecT[N], Point[N]] = identity

  extension [N: Integral](vec: VecT[N])
    def reduce: VecT[N] =
      if vec.x == `0` then (`0`, vec.y.sign)
      else if vec.y == `0` then (vec.x.sign, `0`)
      else
        val gcd = vec.x.abs.gcd(vec.y.abs)
        (vec.x / gcd, vec.y / gcd)

extension [T](seq: Seq[Seq[T]])
  def apply(coord: Coord): T = seq(coord.row)(coord.col)

  def hasCoord(coord: Coord): Boolean =
    seq.indices.contains(coord.row) && seq(coord.row).indices.contains(coord.col)

extension (seq: Seq[String])(using String => WrappedString)
  def apply(coord: Coord): Char = seq(coord.row)(coord.col)

  def hasCoord(coord: Coord): Boolean =
    seq.indices.contains(coord.row) && seq(coord.row).indices.contains(coord.col)

extension [V](grid: Map[Coord, V])
  def toGridAdjacencyList: UnitGraph[Coord] =
    grid.map((coord, _) => (coord, coord.adjacent.filter(grid.contains)))
