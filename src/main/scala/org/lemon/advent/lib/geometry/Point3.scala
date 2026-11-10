package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Point3:
  given [T: Integral]: Conversion[(T, T, T), Point3[T]] =
    (coord: (T, T, T)) => Point3(x = coord._1, y = coord._2, z = coord._3)

  given [T: Integral]: Ordering[Point3[T]] = Ordering.by[Point3[T], T](_.x).orElseBy(_.y).orElseBy(_.z)

  def origin[T: Integral]: Point3[T] = Point3(`0`, `0`, `0`)
  def unitUp[T: Integral]: Point3[T] = origin[T].up
  def unitDown[T: Integral]: Point3[T] = origin[T].down
  def unitLeft[T: Integral]: Point3[T] = origin[T].left
  def unitRight[T: Integral]: Point3[T] = origin[T].right
  def unitIn[T: Integral]: Point3[T] = origin[T].in
  def unitOut[T: Integral]: Point3[T] = origin[T].out

/** A 3d coordinate (or vector) with any integral values.
  * @param x the x coordinate
  * @param y the y coordinate
  * @param z the z coordinate
  */
case class Point3[N: Integral](x: N, y: N, z: N):
  def up: Point3[N] = shiftUp(`1`)
  def down: Point3[N] = shiftDown(`1`)
  def left: Point3[N] = shiftLeft(`1`)
  def right: Point3[N] = shiftRight(`1`)
  def in: Point3[N] = shiftIn(`1`)
  def out: Point3[N] = shiftOut(`1`)
  def move(orientation: Orientation): Point3[N] = shift(orientation, `1`)

  def shiftUp(n: N): Point3[N] = copy(y = y - n)
  def shiftDown(n: N): Point3[N] = copy(y = y + n)
  def shiftLeft(n: N): Point3[N] = copy(x = x - n)
  def shiftRight(n: N): Point3[N] = copy(x = x + n)
  def shiftIn(n: N): Point3[N] = copy(z = z + n)
  def shiftOut(n: N): Point3[N] = copy(z = z - n)
  def shift(orientation: Orientation, n: N): Point3[N] = orientation.facing.runtimeChecked match
    case Point3(0, -1, 0) => shiftUp(n)
    case Point3(0, 1, 0) => shiftDown(n)
    case Point3(-1, 0, 0) => shiftLeft(n)
    case Point3(1, 0, 0) => shiftRight(n)
    case Point3(0, 0, 1) => shiftIn(n)
    case Point3(0, 0, -1) => shiftOut(n)

  def adjacent: Seq[Point3[N]] = Seq(up, down, left, right, in, out)
  def surrounding: Seq[Point3[N]] =
    for
      vertical <- Seq(this, up, down)
      horizontal <- Seq(vertical, vertical.left, vertical.right)
      point <- Seq(horizontal, horizontal.in, horizontal.out)
      if point != this
    yield point

  def manhattan(rhs: Point3[N]): N = (x - rhs.x).abs + (y - rhs.y).abs + (z - rhs.z).abs
  def chessboard(rhs: Point3[N]): N = (x - rhs.x).abs max (y - rhs.y).abs max (z - rhs.z).abs
  def euclidean(rhs: Point3[N]): N =
    val (dx, dy, dz) = (x - rhs.x, y - rhs.y, z - rhs.z)
    dx * dx + dy * dy + dz * dz

  def xInterval(rhs: Point3[N]): Interval[N] = Interval(x min rhs.x, x max rhs.x)
  def yInterval(rhs: Point3[N]): Interval[N] = Interval(y min rhs.y, y max rhs.y)
  def zInterval(rhs: Point3[N]): Interval[N] = Interval(z min rhs.z, z max rhs.z)
  def bounding(rhs: Point3[N]): Cuboid[N] =
    Cuboid(xRange = xInterval(rhs), yRange = yInterval(rhs), zRange = zInterval(rhs))

  def +(rhs: Point3[N]): Point3[N] = Point3(x + rhs.x, y + rhs.y, z + rhs.z)
  def +(orientation: Orientation): Point3[N] = move(orientation)
  def -(rhs: Point3[N]): Point3[N] = Point3(x - rhs.x, y - rhs.y, z - rhs.z)
  def -(orientation: Orientation): Point3[N] = move(orientation.turnAround)
  def *(n: N): Point3[N] = Point3(x * n, y * n, z * n)
  def unary_- : Point3[N] = Point3(-x, -y, -z)

  def dot(rhs: Point3[N]): N = x * rhs.x + y * rhs.y + z * rhs.z
  def cross(rhs: Point3[N]): Point3[N] = Point3(y * rhs.z - z * rhs.y, z * rhs.x - x * rhs.z, x * rhs.y - y * rhs.x)

  /** Reduces this vector to the smallest integer vector in the same direction.
    * @return the reduced vector
    */
  def reduce: Point3[N] =
    val gcd = x.gcd(y).gcd(z)
    if gcd == `0` then this
    else
      val reduced = Point3(x / gcd, y / gcd, z / gcd)
      if gcd < `0` then -reduced else reduced

  def xy: Point[N] = Point(x, y)
  def xz: Point[N] = Point(x, z)
  def yz: Point[N] = Point(y, z)

  def to[M: Integral]: Point3[M] =
    Point3[M](fromString[M](x.toString), fromString[M](y.toString), fromString[M](z.toString))
