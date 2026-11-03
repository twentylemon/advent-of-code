package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Point:
  given [T: Integral]: Conversion[(T, T), Point[T]] = (coord: (T, T)) => Point(x = coord._1, y = coord._2)

  given [T: Integral]: Ordering[Point[T]] = Ordering.by[Point[T], T](_.x).orElseBy(_.y)

  def origin[T: Integral]: Point[T] = Point(`0`, `0`)
  def unitUp[T: Integral]: Point[T] = origin[T].up
  def unitDown[T: Integral]: Point[T] = origin[T].down
  def unitLeft[T: Integral]: Point[T] = origin[T].left
  def unitRight[T: Integral]: Point[T] = origin[T].right

/** A 2d coordinate (or vector) with any integral values. More typical to use [[Coord]],
  * but sometimes you need a `Long`.
  * @param x the x coordinate
  * @param y the y coordinate
  */
case class Point[N: Integral](x: N, y: N):
  def row = y
  def col = x

  def up: Point[N] = shiftUp(`1`)
  def down: Point[N] = shiftDown(`1`)
  def left: Point[N] = shiftLeft(`1`)
  def right: Point[N] = shiftRight(`1`)
  def move(direction: Direction): Point[N] = shift(direction, `1`)

  def shiftUp(n: N): Point[N] = copy(y = y - n)
  def shiftDown(n: N): Point[N] = copy(y = y + n)
  def shiftLeft(n: N): Point[N] = copy(x = x - n)
  def shiftRight(n: N): Point[N] = copy(x = x + n)
  def shift(direction: Direction, n: N): Point[N] = direction match
    case Direction.Up => shiftUp(n)
    case Direction.Down => shiftDown(n)
    case Direction.Left => shiftLeft(n)
    case Direction.Right => shiftRight(n)

  def walk(step: Point[N]): Iterator[Point[N]] = Iterator.iterate(this)(_ + step)
  def walk(direction: Direction): Iterator[Point[N]] = Iterator.iterate(this)(_.move(direction))

  def adjacent: Seq[Point[N]] = Seq(up, down, left, right)
  def surrounding: Seq[Point[N]] = Seq(up, down, left, right, up.left, up.right, down.left, down.right)

  def manhattan(rhs: Point[N]): N = (x - rhs.x).abs + (y - rhs.y).abs
  def chessboard(rhs: Point[N]): N = (x - rhs.x).abs max (y - rhs.y).abs
  def euclidean(rhs: Point[N]): N =
    val (dx, dy) = (x - rhs.x, y - rhs.y)
    dx * dx + dy * dy

  /** Returns an iterator of all points within `distance` of this point using the given metric.
    * This checks all points in a square around the point, assuming that for each row, the metric
    * is symmetric in `dx` and grows with `|dx|` (eg manhattan, chessboard, squared euclidean).
    * @param distance the maximum distance, inclusive
    * @param metric the distance function to use
    * @return iterator of all points within `distance` of this point, including the point itself
    */
  def within(distance: N, metric: (Point[N], Point[N]) => N): Iterator[Point[N]] =
    for
      dy <- Iterator.iterate(-distance)(_ + `1`).takeWhile(_ <= distance)
      max <- Iterator.iterate(distance)(_ - `1`).takeWhile(_ >= `0`)
        .find(dx => metric(this, Point(x + dx, y + dy)) <= distance)
        .iterator
      dx <- Iterator.iterate(-max)(_ + `1`).takeWhile(_ <= max)
    yield Point(x + dx, y + dy)

  def xInterval(rhs: Point[N]): Interval[N] = Interval(x min rhs.x, x max rhs.x)
  def yInterval(rhs: Point[N]): Interval[N] = Interval(y min rhs.y, y max rhs.y)
  def bounding(rhs: Point[N]): Rect[N] = Rect(xRange = xInterval(rhs), yRange = yInterval(rhs))

  def flip = Point(x = y, y = x)

  def rotateRight: Point[N] = Point(x = -y, y = x)
  def rotateRight(about: Point[N]): Point[N] = (this - about).rotateRight + about
  def rotateLeft: Point[N] = Point(x = y, y = -x)
  def rotateLeft(about: Point[N]): Point[N] = (this - about).rotateLeft + about

  def +(rhs: Point[N]): Point[N] = Point(x + rhs.x, y + rhs.y)
  def +(direction: Direction): Point[N] = move(direction)
  def -(rhs: Point[N]): Point[N] = Point(x - rhs.x, y - rhs.y)
  def -(direction: Direction): Point[N] = move(direction.turnAround)
  def *(n: N): Point[N] = Point(x * n, y * n)
  def unary_- : Point[N] = Point(-x, -y)

  /** Returns the 2d cross product. It is positive when `rhs` turns clockwise from this vector, negative when it
    * turns counterclockwise, and `0` when the two are collinear.
    * @param rhs the other vector
    * @return the 2d cross product
    */
  def cross(rhs: Point[N]): N = x * rhs.y - y * rhs.x

  /** Reduces this vector to the smallest integer vector in the same direction.
    * @return the reduced vector
    */
  def reduce: Point[N] =
    if x == `0` then Point(`0`, y.sign)
    else if y == `0` then Point(x.sign, `0`)
    else
      val gcd = x.gcd(y)
      val reduced = Point(x / gcd, y / gcd)
      if gcd < `0` then -reduced else reduced

  def directionTo(rhs: Point[N]): Option[Direction] =
    val Point(dx, dy) = rhs - this
    if dx == `0` && dy < `0` then Some(Direction.Up)
    else if dx == `0` && dy > `0` then Some(Direction.Down)
    else if dx < `0` && dy == `0` then Some(Direction.Left)
    else if dx > `0` && dy == `0` then Some(Direction.Right)
    else None

  def to[M: Integral]: Point[M] = Point[M](fromString[M](x.toString), fromString[M](y.toString))

extension [N: Integral](vertices: Seq[Point[N]])
  /** Computes the area of a polygon using the shoelace formula.
    * @return the area of the polygon
    */
  def shoelaceArea: N = vertices
    .lazyZip(vertices.tail :+ vertices.head)
    .map(_ `cross` _)
    .reduce(_ + _)
    .abs / Integral[N].fromInt(2)
