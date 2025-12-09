package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Point:
  given [T: Integral]: Conversion[(T, T), Point[T]] = (coord: (T, T)) => Point(x = coord._1, y = coord._2)

  given [T: Integral]: Ordering[Point[T]] = Ordering.by[Point[T], T](_.x).orElseBy(_.y)

  def origin[T: Integral]: Point[T] = (`0`, `0`)
  def unitUp[T: Integral]: Point[T] = (`0`, `-1`)
  def unitDown[T: Integral]: Point[T] = (`0`, `1`)
  def unitLeft[T: Integral]: Point[T] = (`-1`, `0`)
  def unitRight[T: Integral]: Point[T] = (`1`, `0`)

/** A 2d coordinate (or vector) with any integral values. More typical to use [[Coord]],
  * but sometimes you need a `Long`.
  * @param x the x coordinate
  * @param y the y coordinate
  */
case class Point[N: Integral](x: N, y: N):
  def row = y
  def col = x

  def up: Point[N] = copy(y = y - `1`)
  def down: Point[N] = copy(y = y + `1`)
  def left: Point[N] = copy(x = x - `1`)
  def right: Point[N] = copy(x = x + `1`)
  def move(direction: Direction): Point[N] = (x + fromInt(direction.unitVector.x), y + fromInt(direction.unitVector.y))

  def shiftUp(n: N): Point[N] = copy(y = y - n)
  def shiftDown(n: N): Point[N] = copy(y = y + n)
  def shiftLeft(n: N): Point[N] = copy(x = x - n)
  def shiftRight(n: N): Point[N] = copy(x = x + n)
  def shift(direction: Direction, n: N): Point[N] =
    (x + fromInt(direction.unitVector.x) * n, y + fromInt(direction.unitVector.y) * n)

  def walk(direction: Direction): Iterator[Point[N]] = Iterator.iterate(this)(_.move(direction))

  def adjacent: Seq[Point[N]] = Seq(up, down, left, right)
  def surrounding: Seq[Point[N]] = Seq(up, down, left, right, up.left, up.right, down.left, down.right)

  def manhattan(rhs: Point[N]): N = (x - rhs.x).abs + (y - rhs.y).abs
  def chessboard(rhs: Point[N]): N = (x - rhs.x).abs max (y - rhs.y).abs

  /** Returns an iterator of all points within `distance` of this point using the given metric.
    * This checks all points in a square around the point, so will not be correct if
    * `metric(point, point.shift(direction, distance) > distance` (eg squared euclidean distance).
    * @param distance the maximum distance, inclusive
    * @param metric the distance function to use
    * @return iterator of all points within `distance` of this point, including the point itself
    */
  def within(distance: N, metric: (Point[N], Point[N]) => N): Iterator[Point[N]] =
    for
      dy <- Iterator.iterate(-distance)(_ + `1`).takeWhile(_ <= distance)
      dx <- Iterator.iterate(-distance)(_ + `1`).takeWhile(_ <= distance)
      p = Point(x + dx, y + dy)
      if metric(this, p) <= distance
    yield p

  def xInterval(rhs: Point[N]): Interval[N] = Interval(x min rhs.x, x max rhs.x)
  def yInterval(rhs: Point[N]): Interval[N] = Interval(y min rhs.y, y max rhs.y)
  def bounding(rhs: Point[N]): Rect[N] = Rect(xRange = xInterval(rhs), yRange = yInterval(rhs))

  def shiftInto(area: Area): Coord =
    ((x +% fromInt(area.width)).toInt + area.left, (y +% fromInt(area.height)).toInt + area.top)

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
