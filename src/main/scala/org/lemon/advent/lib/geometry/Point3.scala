package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Point3:
  given [T: Integral]: Conversion[(T, T, T), Point3[T]] =
    (coord: (T, T, T)) => Point3(x = coord._1, y = coord._2, z = coord._3)

  given [T: Integral]: Ordering[Point3[T]] = Ordering.by[Point3[T], T](_.x).orElseBy(_.y).orElseBy(_.z)

  def origin[T: Integral]: Point3[T] = Point3(`0`, `0`, `0`)
  def unitX[T: Integral]: Point3[T] = Point3(`1`, `0`, `0`)
  def unitY[T: Integral]: Point3[T] = Point3(`0`, `1`, `0`)
  def unitZ[T: Integral]: Point3[T] = Point3(`0`, `0`, `1`)

  // the 24 rotations, as the (source axis, sign) of each output component, starting with the identity
  private val rotationMatrices: Seq[Seq[(Int, Int)]] =
    def parity(perm: Seq[Int]) =
      val inversions = perm.indices.combinations(2).count { case Seq(i, j) => perm(i) > perm(j) }
      if inversions % 2 == 0 then 1 else -1
    val signs = for a <- Seq(1, -1); b <- Seq(1, -1); c <- Seq(1, -1) yield Seq(a, b, c)
    for
      perm <- Seq(0, 1, 2).permutations.toSeq
      sign <- signs
      if parity(perm) * sign.product == 1
    yield perm.zip(sign)

/** A 3d coordinate (or vector) with any integral values.
  * @param x the x coordinate
  * @param y the y coordinate
  * @param z the z coordinate
  */
case class Point3[N: Integral](x: N, y: N, z: N):
  def up: Point3[N] = copy(y = y - `1`)
  def down: Point3[N] = copy(y = y + `1`)
  def left: Point3[N] = copy(x = x - `1`)
  def right: Point3[N] = copy(x = x + `1`)

  /** @return this point moved one step along +z */
  def in: Point3[N] = copy(z = z + `1`)

  /** @return this point moved one step along -z */
  def out: Point3[N] = copy(z = z - `1`)

  /** @return the 6 points that share a face with this point */
  def adjacent: Seq[Point3[N]] = Seq(up, down, left, right, in, out)

  /** @return the 26 points that share a face, edge or corner with this point */
  def surrounding: Seq[Point3[N]] =
    for
      vertical <- Seq(this, up, down)
      horizontal <- Seq(vertical, vertical.left, vertical.right)
      point <- Seq(horizontal, horizontal.in, horizontal.out)
      if point != this
    yield point

  def manhattan(rhs: Point3[N]): N = (x - rhs.x).abs + (y - rhs.y).abs + (z - rhs.z).abs
  def chessboard(rhs: Point3[N]): N = (x - rhs.x).abs max (y - rhs.y).abs max (z - rhs.z).abs

  /** @return the squared euclidean distance to `rhs` */
  def euclidean(rhs: Point3[N]): N =
    val (dx, dy, dz) = (x - rhs.x, y - rhs.y, z - rhs.z)
    dx * dx + dy * dy + dz * dz

  def xInterval(rhs: Point3[N]): Interval[N] = Interval(x min rhs.x, x max rhs.x)
  def yInterval(rhs: Point3[N]): Interval[N] = Interval(y min rhs.y, y max rhs.y)
  def zInterval(rhs: Point3[N]): Interval[N] = Interval(z min rhs.z, z max rhs.z)

  /** Returns this point under each of the 24 rotations about the origin. The rotations are in the same order
    * for every point, starting with the identity.
    * @return this point under each rotation
    */
  def rotations: Seq[Point3[N]] =
    val axes = IndexedSeq(x, y, z)
    def component(axis: Int, sign: Int) = if sign > 0 then axes(axis) else -axes(axis)
    Point3.rotationMatrices.map(matrix =>
      val components = matrix.map(component.tupled)
      Point3(components(0), components(1), components(2))
    )

  def +(rhs: Point3[N]): Point3[N] = Point3(x + rhs.x, y + rhs.y, z + rhs.z)
  def -(rhs: Point3[N]): Point3[N] = Point3(x - rhs.x, y - rhs.y, z - rhs.z)
  def *(n: N): Point3[N] = Point3(x * n, y * n, z * n)
  def unary_- : Point3[N] = Point3(-x, -y, -z)

  def dot(rhs: Point3[N]): N = x * rhs.x + y * rhs.y + z * rhs.z

  /** Returns the cross product of the two vectors.
    * @param rhs the other vector
    * @return the cross product
    */
  def cross(rhs: Point3[N]): Point3[N] = Point3(y * rhs.z - z * rhs.y, z * rhs.x - x * rhs.z, x * rhs.y - y * rhs.x)

  /** Reduces this vector to the smallest integer vector in the same direction.
    * @return the reduced vector
    */
  def reduce: Point3[N] =
    val gcd = x.gcd(y).gcd(z)
    if gcd == `0` then this
    else
      // the gcd overflows to `MinValue` when it should be `-MinValue`, which would flip the direction
      val reduced = Point3(x / gcd, y / gcd, z / gcd)
      if gcd < `0` then -reduced else reduced

  def xy: Point[N] = Point(x, y)
  def xz: Point[N] = Point(x, z)
  def yz: Point[N] = Point(y, z)

  def to[M: Integral]: Point3[M] =
    Point3[M](fromString[M](x.toString), fromString[M](y.toString), fromString[M](z.toString))
