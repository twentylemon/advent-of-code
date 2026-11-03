package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Point3:
  given [T: Integral]: Conversion[(T, T, T), Point3[T]] =
    (coord: (T, T, T)) => Point3(x = coord._1, y = coord._2, z = coord._3)

  given [T: Integral]: Ordering[Point3[T]] = Ordering.by[Point3[T], T](_.x).orElseBy(_.y).orElseBy(_.z)

  def origin[T: Integral]: Point3[T] = (`0`, `0`, `0`)
  def unitX[T: Integral]: Point3[T] = (`1`, `0`, `0`)
  def unitY[T: Integral]: Point3[T] = (`0`, `1`, `0`)
  def unitZ[T: Integral]: Point3[T] = (`0`, `0`, `1`)

  /** The 24 rotations that map the axes onto the axes, as the signed permutation matrices with determinant 1.
    * Each rotation gives the (source axis, sign) of each output component. The identity is first.
    */
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

/** A 3d coordinate (or vector) with any integral values. More typical to use [[Coord3]],
  * but sometimes you need a `Long`.
  * @param x the x coordinate
  * @param y the y coordinate
  * @param z the z coordinate
  */
case class Point3[N: Integral](x: N, y: N, z: N):
  def +(rhs: Point3[N]): Point3[N] = (x + rhs.x, y + rhs.y, z + rhs.z)
  def -(rhs: Point3[N]): Point3[N] = (x - rhs.x, y - rhs.y, z - rhs.z)
  def *(n: N): Point3[N] = (x * n, y * n, z * n)
  def unary_- : Point3[N] = (-x, -y, -z)

  /** @return the 6 points that share a face with this point */
  def adjacent: Seq[Point3[N]] = Seq(
    copy(x = x - `1`),
    copy(x = x + `1`),
    copy(y = y - `1`),
    copy(y = y + `1`),
    copy(z = z - `1`),
    copy(z = z + `1`),
  )

  /** @return the 26 points that share a face, edge or corner with this point */
  def surrounding: Seq[Point3[N]] =
    val deltas = Seq[N](`-1`, `0`, `1`)
    for
      dx <- deltas
      dy <- deltas
      dz <- deltas
      if dx != `0` || dy != `0` || dz != `0`
    yield Point3(x + dx, y + dy, z + dz)

  def manhattan(rhs: Point3[N]): N = (x - rhs.x).abs + (y - rhs.y).abs + (z - rhs.z).abs
  def chessboard(rhs: Point3[N]): N = (x - rhs.x).abs max (y - rhs.y).abs max (z - rhs.z).abs

  /** @return the squared euclidean distance to `rhs` */
  def euclidean(rhs: Point3[N]): N =
    val (dx, dy, dz) = (x - rhs.x, y - rhs.y, z - rhs.z)
    dx * dx + dy * dy + dz * dz

  def dot(rhs: Point3[N]): N = x * rhs.x + y * rhs.y + z * rhs.z

  /** Returns the cross product, which is perpendicular to both vectors, following the right hand rule.
    * @param rhs the other vector
    * @return the cross product
    */
  def cross(rhs: Point3[N]): Point3[N] = (y * rhs.z - z * rhs.y, z * rhs.x - x * rhs.z, x * rhs.y - y * rhs.x)

  /** Reduces this vector to the smallest integer step in the same direction, by dividing out the gcd
    * of its components. Axis aligned vectors become unit vectors, and the origin stays the origin.
    * @return the smallest integer vector in the same direction
    */
  def reduce: Point3[N] =
    val gcd = x.gcd(y).gcd(z)
    if gcd == `0` then this
    else
      // the gcd overflows to `MinValue` when it should be `-MinValue`, which would flip the direction
      val reduced = Point3(x / gcd, y / gcd, z / gcd)
      if gcd < `0` then -reduced else reduced

  /** Returns this point under each of the 24 rotations about the origin that map the axes onto the axes,
    * ie every way to orient a cube. The order is fixed, so the rotation at index `i` is the same
    * rotation for every point. The first rotation is the identity.
    * @return this point under each of the 24 rotations
    */
  def rotations: Seq[Point3[N]] =
    val axes = IndexedSeq(x, y, z)
    def component(axis: Int, sign: Int) = if sign > 0 then axes(axis) else -axes(axis)
    Point3.rotationMatrices.map(matrix =>
      val components = matrix.map(component.tupled)
      Point3(components(0), components(1), components(2))
    )

  def xInterval(rhs: Point3[N]): Interval[N] = Interval(x min rhs.x, x max rhs.x)
  def yInterval(rhs: Point3[N]): Interval[N] = Interval(y min rhs.y, y max rhs.y)
  def zInterval(rhs: Point3[N]): Interval[N] = Interval(z min rhs.z, z max rhs.z)
  def bounding(rhs: Point3[N]): Cuboid[N] = Cuboid(xInterval(rhs), yInterval(rhs), zInterval(rhs))

  def xy: Point[N] = Point(x, y)
  def xz: Point[N] = Point(x, z)
  def yz: Point[N] = Point(y, z)

  def to[M: Integral]: Point3[M] =
    Point3[M](fromString[M](x.toString), fromString[M](y.toString), fromString[M](z.toString))
