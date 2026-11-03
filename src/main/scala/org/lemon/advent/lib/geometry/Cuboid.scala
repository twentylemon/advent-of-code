package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Cuboid:
  /** @return the smallest cuboid enclosing all the points */
  def apply[N: Integral](points: Iterable[Point3[N]]): Cuboid[N] =
    val (xs, ys, zs) = (points.map(_.x), points.map(_.y), points.map(_.z))
    Cuboid(Interval(xs.min, xs.max), Interval(ys.min, ys.max), Interval(zs.min, zs.max))

  def apply(xRange: Range, yRange: Range, zRange: Range): Cuboid[Int] =
    Cuboid(xRange.toInterval, yRange.toInterval, zRange.toInterval)

  /** @return a cuboid of the given size, with its minimum corner at the origin */
  def apply[N: Integral](width: N, height: N, depth: N): Cuboid[N] =
    Cuboid(Interval(`0`, width - `1`), Interval(`0`, height - `1`), Interval(`0`, depth - `1`))

  given [N: Integral]: Conversion[Cuboid[N], Iterator[Point3[N]]] = _.iterator

/** A 3d cuboid (rectangular box) with generic integral bounds. Empty when any of its ranges are empty.
  * @param xRange the x bounds of the cuboid
  * @param yRange the y bounds of the cuboid
  * @param zRange the z bounds of the cuboid
  * @tparam N the numeric type for coordinates
  */
case class Cuboid[N: Integral](xRange: Interval[N], yRange: Interval[N], zRange: Interval[N]):
  def min: Point3[N] = Point3(xRange.min, yRange.min, zRange.min)
  def max: Point3[N] = Point3(xRange.max, yRange.max, zRange.max)

  /** @return the 8 corners, which repeat when the cuboid is one wide along some axis */
  def corners: Seq[Point3[N]] =
    for
      x <- Seq(xRange.min, xRange.max)
      y <- Seq(yRange.min, yRange.max)
      z <- Seq(zRange.min, zRange.max)
    yield Point3(x, y, z)

  def width: N = xRange.length
  def height: N = yRange.length
  def depth: N = zRange.length

  def volume: N = width * height * depth

  /** @return the total area of the 6 faces, ie the number of unit faces on the outside of the cuboid */
  def surfaceArea: N =
    if isEmpty then `0` else fromInt[N](2) * (width * height + height * depth + width * depth)

  def isEmpty: Boolean = xRange.isEmpty || yRange.isEmpty || zRange.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def contains(point: Point3[N]): Boolean =
    xRange.contains(point.x) && yRange.contains(point.y) && zRange.contains(point.z)
  def apply(point: Point3[N]): Boolean = contains(point)

  /** @return all the points in the cuboid, by z, then y, then x */
  def iterator: Iterator[Point3[N]] =
    for z <- zRange.iterator; y <- yRange.iterator; x <- xRange.iterator yield Point3(x, y, z)

  def encloses(rhs: Cuboid[N]): Boolean =
    rhs.isEmpty ||
      (nonEmpty && xRange.containsSlice(rhs.xRange) && yRange.containsSlice(rhs.yRange) &&
        zRange.containsSlice(rhs.zRange))

  def overlaps(rhs: Cuboid[N]): Boolean =
    xRange.intersects(rhs.xRange) && yRange.intersects(rhs.yRange) && zRange.intersects(rhs.zRange)
  def intersects(rhs: Cuboid[N]): Boolean = overlaps(rhs)

  def intersect(rhs: Cuboid[N]): Cuboid[N] = Cuboid(xRange & rhs.xRange, yRange & rhs.yRange, zRange & rhs.zRange)
  def &(rhs: Cuboid[N]): Cuboid[N] = intersect(rhs)

  /** Returns the points in this cuboid that are not in `rhs`, as at most 6 disjoint non-empty cuboids.
    * @param rhs the cuboid to remove
    * @return disjoint cuboids covering exactly the points in this cuboid but not in `rhs`
    */
  def diff(rhs: Cuboid[N]): Seq[Cuboid[N]] =
    val common = intersect(rhs)
    if isEmpty then Seq.empty
    else if common.isEmpty then Seq(this)
    else
      // the parts of `range` either side of `inner`, only made when there is room so the bounds cannot overflow
      def outside(range: Interval[N], inner: Interval[N]): Seq[Interval[N]] =
        Option.when(inner.start > range.start)(Interval(range.start, inner.start - `1`)).toSeq ++
          Option.when(inner.end < range.end)(Interval(inner.end + `1`, range.end)).toSeq

      // slabs either side along x, then y within the common x, then z within the common x and y
      outside(xRange, common.xRange).map(x => Cuboid(x, yRange, zRange)) ++
        outside(yRange, common.yRange).map(y => Cuboid(common.xRange, y, zRange)) ++
        outside(zRange, common.zRange).map(z => Cuboid(common.xRange, common.yRange, z))

  def --(rhs: Cuboid[N]): Seq[Cuboid[N]] = diff(rhs)

  /** Splits this cuboid in half along each axis. If the cuboid has an odd length along an axis,
    * the middle slice is in the lower half. Halves that would be empty are left out, so a cuboid one
    * wide along an axis is not split along it.
    * @return the non-empty octants of this cuboid
    */
  def octants: Seq[Cuboid[N]] =
    def halves(range: Interval[N]): Seq[Interval[N]] =
      val mid = range.start + (range.length - `1`) / fromInt(2)
      Interval(range.start, mid) +: Option.when(mid < range.end)(Interval(mid + `1`, range.end)).toSeq

    if isEmpty then Seq.empty
    else for x <- halves(xRange); y <- halves(yRange); z <- halves(zRange) yield Cuboid(x, y, z)

  def expand(n: N): Cuboid[N] = Cuboid(grow(xRange, n), grow(yRange, n), grow(zRange, n))
  def contract(n: N): Cuboid[N] = expand(-n)
  private def grow(range: Interval[N], n: N) = Interval(range.start - n, range.end + n)

  def +(offset: Point3[N]): Cuboid[N] =
    Cuboid(shift(xRange, offset.x), shift(yRange, offset.y), shift(zRange, offset.z))
  def -(offset: Point3[N]): Cuboid[N] = this + -offset
  private def shift(range: Interval[N], n: N) = Interval(range.start + n, range.end + n)

  def clamp(point: Point3[N]): Point3[N] = Point3(
    point.x max xRange.min min xRange.max,
    point.y max yRange.min min yRange.max,
    point.z max zRange.min min zRange.max,
  )

  def xy: Rect[N] = Rect(xRange, yRange)
  def xz: Rect[N] = Rect(xRange, zRange)
  def yz: Rect[N] = Rect(yRange, zRange)
