package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*
import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*
import scala.collection.immutable.WrappedString

object Rect:
  def apply[N: Integral](grid: Map[Point[N], ?]): Rect[N] = apply(grid.keySet)

  def apply[N: Integral](coords: Iterable[Point[N]]): Rect[N] =
    val (xs, ys) = (coords.map(_.x), coords.map(_.y))
    Rect(xRange = Interval(xs.min, xs.max), yRange = Interval(ys.min, ys.max))

  def apply(grid: Seq[Seq[?]]): Rect[Int] =
    Rect(yRange = grid.indices.toInterval, xRange = grid.head.indices.toInterval)

  def apply(lines: Seq[String])(using String => WrappedString): Rect[Int] =
    Rect(yRange = lines.indices.toInterval, xRange = lines.head.indices.toInterval)

  def apply[N: Integral](left: N, right: N, top: N, bottom: N): Rect[N] =
    Rect(Interval(left, right), Interval(top, bottom))

  def apply[N: Integral](width: N, height: N): Rect[N] =
    apply(`0`, width - `1`, `0`, height - `1`)

  given [N: Integral]: Conversion[Rect[N], Iterator[Point[N]]] = _.iterator

/** A 2d rectangle with generic integral bounds.
  * @param xRange the x bounds of the rectangle
  * @param yRange the y bounds of the rectangle
  * @tparam N the numeric type for coordinates
  */
case class Rect[N: Integral](xRange: Interval[N], yRange: Interval[N]):
  def left: N = xRange.min
  def right: N = xRange.max
  def top: N = yRange.min
  def bottom: N = yRange.max

  def topLeft: Point[N] = Point(left, top)
  def bottomRight: Point[N] = Point(right, bottom)
  def topRight: Point[N] = Point(right, top)
  def bottomLeft: Point[N] = Point(left, bottom)
  def corners: Seq[Point[N]] = Seq(topLeft, topRight, bottomRight, bottomLeft)

  def width: N = xRange.length
  def height: N = yRange.length

  def size: N = width * height

  /** @return the total length of the rectangle's edges */
  def perimeter: N = if isEmpty then `0` else fromInt[N](2) * (width + height)

  def isEmpty: Boolean = xRange.isEmpty || yRange.isEmpty
  def nonEmpty: Boolean = xRange.nonEmpty && yRange.nonEmpty

  def contains(coord: Point[N]): Boolean = xRange.contains(coord.x) && yRange.contains(coord.y)
  def apply(coord: Point[N]): Boolean = contains(coord)

  /** @return all the points in the rectangle, row by row from the top left */
  def iterator: Iterator[Point[N]] = for y <- yRange.iterator; x <- xRange.iterator yield Point(x, y)

  def col(x: N): Rect[N] =
    require(xRange.contains(x), s"$this does not contain column $x")
    copy(xRange = Interval(x, x))

  def reverseCol(x: N): Iterator[Point[N]] =
    require(xRange.contains(x), s"$this does not contain column $x")
    for y <- yRange.reverseIterator yield Point(x, y)

  def leftCol: Rect[N] = col(left)
  def rightCol: Rect[N] = col(right)

  def row(y: N): Rect[N] =
    require(yRange.contains(y), s"$this does not contain row $y")
    copy(yRange = Interval(y, y))

  def reverseRow(y: N): Iterator[Point[N]] =
    require(yRange.contains(y), s"$this does not contain row $y")
    for x <- xRange.reverseIterator yield Point(x, y)

  def topRow: Rect[N] = row(top)
  def bottomRow: Rect[N] = row(bottom)

  def boundary: Iterator[Point[N]] =
    if isEmpty then Iterator.empty
    else if width == `1` then leftCol.iterator
    else if height == `1` then topRow.iterator
    else
      topRow.iterator ++ rightCol.iterator.drop(1) ++ reverseRow(bottom).drop(1) ++
        reverseCol(left).drop(1).take(height.toInt - 2)

  def upDiagonal(startAt: Point[N]): Iterator[Point[N]] =
    require(contains(startAt), s"$this does not contain startAt $startAt")
    Iterator.iterate(startAt)(_.up.right).takeWhile(contains)

  def downDiagonal(startAt: Point[N]): Iterator[Point[N]] =
    require(contains(startAt), s"$this does not contain startAt $startAt")
    Iterator.iterate(startAt)(_.down.right).takeWhile(contains)

  def rows: Iterator[Rect[N]] = yRange.iterator.map(row)
  def cols: Iterator[Rect[N]] = xRange.iterator.map(col)
  def upDiagonals: Iterator[Iterator[Point[N]]] =
    xRange.iterator.map(x => upDiagonal(Point(x, bottom))) ++
      yRange.reverseIterator.drop(1).map(y => upDiagonal(Point(left, y)))
  def downDiagonals: Iterator[Iterator[Point[N]]] =
    xRange.iterator.map(x => downDiagonal(Point(x, top))) ++
      yRange.iterator.drop(1).map(y => downDiagonal(Point(left, y)))
  def diagonals: Iterator[Iterator[Point[N]]] = upDiagonals ++ downDiagonals

  /** Iterates over _all_ (width x height) areas within this area.
    * Areas returned will overlap each other, it's _every_ sub-area.
    * @param width the sub-area width
    * @param height the sub-area height
    * @return iterator of all sub-areas of the given size in this area
    */
  def rectangles(width: N, height: N): Iterator[Rect[N]] =
    if isEmpty then Iterator.empty
    else
      for
        startX <- xRange.take(this.width - width + `1`).iterator
        startY <- yRange.take(this.height - height + `1`).iterator
      yield Rect(Interval(startX, startX + width - `1`), Interval(startY, startY + height - `1`))

  /** Returns the quadrants of this rectangle. If the rectangle has an odd width or height,
    * the middle column or row is in the left or top quadrants.
    * @return the non-empty quadrants of this rectangle
    */
  def quadrants: Seq[Rect[N]] =
    def halves(range: Interval[N]): Seq[Interval[N]] =
      val mid = range.start + (range.length - `1`) / fromInt(2)
      Interval(range.start, mid) +: Option.when(mid < range.end)(Interval(mid + `1`, range.end)).toSeq

    if isEmpty then Seq.empty
    else for y <- halves(yRange); x <- halves(xRange) yield Rect(x, y)

  def growLeft(n: N): Rect[N] = copy(xRange = xRange.growStart(n))
  def growRight(n: N): Rect[N] = copy(xRange = xRange.growEnd(n))
  def growTop(n: N): Rect[N] = copy(yRange = yRange.growStart(n))
  def growBottom(n: N): Rect[N] = copy(yRange = yRange.growEnd(n))
  def expand(n: N): Rect[N] = Rect(xRange.expand(n), yRange.expand(n))

  def dropLeft(n: N): Rect[N] = growLeft(-n)
  def dropRight(n: N): Rect[N] = growRight(-n)
  def dropTop(n: N): Rect[N] = growTop(-n)
  def dropBottom(n: N): Rect[N] = growBottom(-n)
  def contract(n: N): Rect[N] = Rect(xRange.contract(n), yRange.contract(n))

  def translate(offset: Point[N]): Rect[N] = Rect(xRange.shift(offset.x), yRange.shift(offset.y))

  def encloses(rhs: Rect[N]): Boolean =
    rhs.isEmpty || xRange.containsSlice(rhs.xRange) && yRange.containsSlice(rhs.yRange)

  def overlaps(rhs: Rect[N]): Boolean = xRange.intersects(rhs.xRange) && yRange.intersects(rhs.yRange)
  def intersects(rhs: Rect[N]): Boolean = overlaps(rhs)

  def intersect(rhs: Rect[N]): Rect[N] = Rect(xRange & rhs.xRange, yRange & rhs.yRange)

  def &(rhs: Rect[N]): Rect[N] = intersect(rhs)

  /** Returns the points in this rectangle that are not in `rhs`, as disjoint rectangles.
    * @param rhs the rectangle to remove
    * @return the disjoint rectangles
    */
  def diff(rhs: Rect[N]): Seq[Rect[N]] =
    val common = intersect(rhs)
    if isEmpty then Seq.empty
    else if common.isEmpty then Seq(this)
    else
      def outside(range: Interval[N], inner: Interval[N]): Seq[Interval[N]] =
        Option.when(inner.start > range.start)(Interval(range.start, inner.start - `1`)).toSeq ++
          Option.when(inner.end < range.end)(Interval(inner.end + `1`, range.end)).toSeq

      outside(xRange, common.xRange).map(x => Rect(x, yRange)) ++
        outside(yRange, common.yRange).map(y => Rect(common.xRange, y))

  def --(rhs: Rect[N]): Seq[Rect[N]] = diff(rhs)

  def clamp(coord: Point[N]): Point[N] = Point(xRange.clamp(coord.x), yRange.clamp(coord.y))

  def wrap(coord: Point[N]): Point[N] = Point(xRange.wrap(coord.x), yRange.wrap(coord.y))

  def show(coord2Char: Point[N] => Char): String = rows.map(_.map(coord2Char).mkString + "\n").mkString
