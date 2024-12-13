package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.graph.UnitGraph
import org.lemon.advent.lib._

import scala.collection.immutable.WrappedString
import scala.math.Integral.Implicits._
import scala.math.Ordering.Implicits._

type Coord = CoordT[Int]

object Coord:
  val origin: Coord = (0, 0)
  val unitUp: Coord = (0, -1)
  val unitDown: Coord = (0, 1)
  val unitLeft: Coord = (-1, 0)
  val unitRight: Coord = (1, 0)

  def apply(x: Int, y: Int): Coord = CoordT(x, y)

  def gridToMap(input: String): Map[Coord, Char] = input.linesIterator
    .zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.map((c, x) => (Coord(x, y), c)))
    .toMap

object CoordT:
  given [T: Integral]: Conversion[(T, T), CoordT[T]] = (coord: (T, T)) => CoordT(x = coord._1, y = coord._2)

  given [T: Integral]: Ordering[CoordT[T]] = Ordering.by[CoordT[T], T](_.x).orElseBy(_.y)

  def origin[T: Integral]: CoordT[T] = CoordT(Integral[T].zero, Integral[T].zero)
  def unitUp[T: Integral]: CoordT[T] = CoordT(Integral[T].zero, -Integral[T].one)
  def unitDown[T: Integral]: CoordT[T] = CoordT(Integral[T].zero, Integral[T].one)
  def unitLeft[T: Integral]: CoordT[T] = CoordT(-Integral[T].one, Integral[T].zero)
  def unitRight[T: Integral]: CoordT[T] = CoordT(Integral[T].one, Integral[T].zero)

  def apply[T: Integral](x: Int, y: Int): CoordT[T] = CoordT(Integral[T].fromInt(x), Integral[T].fromInt(y))
  def apply[T: Integral](coord: Coord): CoordT[T] = apply(coord.x, coord.y)

case class CoordT[N: Integral](x: N, y: N):
  def row = y
  def col = x

  def up: CoordT[N] = copy(y = y - Integral[N].one)
  def down: CoordT[N] = copy(y = y + Integral[N].one)
  def left: CoordT[N] = copy(x = x - Integral[N].one)
  def right: CoordT[N] = copy(x = x + Integral[N].one)
  def move(direction: Direction): CoordT[N] = this + CoordT[N](direction.unitVector)

  def shiftUp(n: N): CoordT[N] = copy(y = y - n)
  def shiftDown(n: N): CoordT[N] = copy(y = y + n)
  def shiftLeft(n: N): CoordT[N] = copy(x = x - n)
  def shiftRight(n: N): CoordT[N] = copy(x = x + n)
  def shift(direction: Direction, n: N): CoordT[N] = this + (CoordT[N](direction.unitVector) * n)

  def adjacent: Seq[CoordT[N]] = Seq(up, down, left, right)
  def surrounding: Seq[CoordT[N]] = Seq(up, down, left, right, up.left, up.right, down.left, down.right)

  def manhattan(rhs: CoordT[N]): N = (x - rhs.x).abs + (y - rhs.y).abs

  def xRange(rhs: CoordT[N]): Range = x.min(rhs.x).toInt to x.max(rhs.x).toInt
  def yRange(rhs: CoordT[N]): Range = y.min(rhs.y).toInt to y.max(rhs.y).toInt
  def bounding(rhs: CoordT[N]): Area = Area(xRange = xRange(rhs), yRange = yRange(rhs))

  def flip = CoordT[N](x = y, y = x)

  def +(rhs: CoordT[N]): CoordT[N] = (x + rhs.x, y + rhs.y)
  def +(direction: Direction): CoordT[N] = move(direction)
  def -(rhs: CoordT[N]): CoordT[N] = (x - rhs.x, y - rhs.y)
  def -(direction: Direction): CoordT[N] = move(direction.turnAround)
  def *(n: N): CoordT[N] = (x * n, y * n)

  def directionTo(rhs: CoordT[N]): Option[Direction] =
    val CoordT[N](dx, dy) = rhs - this
    if dx == Integral[N].zero && dy < Integral[N].zero then Some(Direction.Up)
    else if dx == Integral[N].zero && dy > Integral[N].zero then Some(Direction.Down)
    else if dx < Integral[N].zero && dy == Integral[N].zero then Some(Direction.Left)
    else if dx > Integral[N].zero && dy == Integral[N].zero then Some(Direction.Right)
    else None

  def asVec: VecT[N] = VecT[N](this)

type Vec = VecT[Int]
opaque type VecT[N] = CoordT[N]
object VecT:
  def apply[N: Integral](coord: CoordT[N]): VecT[N] = coord
  def apply[N: Integral](x: N, y: N): VecT[N] = CoordT[N](x, y)

  given [N: Integral]: Conversion[VecT[N], CoordT[N]] = identity

  extension [N: Integral](vec: VecT[N])
    def reduce: VecT[N] =
      if vec.x == Integral[N].zero then (Integral[N].zero, vec.y.sign)
      else if vec.y == Integral[N].zero then (vec.x.sign, Integral[N].zero)
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
