package org.lemon.advent.lib.`2d`

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

  given [N: Integral]: Conversion[Rect[N], Iterator[Point[N]]] =
    (rect: Rect[N]) => for y <- rect.yRange.iterator; x <- rect.xRange.iterator yield Point(x, y)

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

  def isEmpty: Boolean = xRange.isEmpty || yRange.isEmpty
  def nonEmpty: Boolean = xRange.nonEmpty && yRange.nonEmpty

  def contains(coord: Point[N]): Boolean = xRange.contains(coord.x) && yRange.contains(coord.y)
  def apply(coord: Point[N]): Boolean = contains(coord)

  def col(x: N): Iterator[Point[N]] =
    require(xRange.contains(x), s"$this does not contain column $x")
    for y <- yRange.iterator yield Point(x, y)

  def reverseCol(x: N): Iterator[Point[N]] =
    require(xRange.contains(x), s"$this does not contain column $x")
    for y <- yRange.reverseIterator yield Point(x, y)

  def leftCol: Iterator[Point[N]] = col(left)
  def rightCol: Iterator[Point[N]] = col(right)

  def row(y: N): Iterator[Point[N]] =
    require(yRange.contains(y), s"$this does not contain row $y")
    for x <- xRange.iterator yield Point(x, y)

  def reverseRow(y: N): Iterator[Point[N]] =
    require(yRange.contains(y), s"$this does not contain row $y")
    for x <- xRange.reverseIterator yield Point(x, y)

  def topRow: Iterator[Point[N]] = row(top)
  def bottomRow: Iterator[Point[N]] = row(bottom)

  def boundary: Iterator[Point[N]] =
    if isEmpty then Iterator.empty
    else if width == `1` then leftCol
    else if height == `1` then topRow
    else topRow ++ rightCol.drop(1) ++ reverseRow(bottom).drop(1) ++ reverseCol(left).drop(1).take(height.toInt - 2)

  def upDiagonal(startAt: Point[N]): Iterator[Point[N]] =
    require(contains(startAt), s"$this does not contain startAt $startAt")
    Iterator.iterate(startAt)(_.up.right).takeWhile(contains)

  def downDiagonal(startAt: Point[N]): Iterator[Point[N]] =
    require(contains(startAt), s"$this does not contain startAt $startAt")
    Iterator.iterate(startAt)(_.down.right).takeWhile(contains)

  def rows: Iterator[Iterator[Point[N]]] = yRange.iterator.map(row)
  def cols: Iterator[Iterator[Point[N]]] = xRange.iterator.map(col)
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

  /** Returns the four quadrants of this area. If the area has an odd width or height,
    * the middle row or column will be in the top or left quadrants.
    * @return the four quadrant areas of this area
    */
  def quadrants: Quadrants[N] =
    val (midX, midY) = (left + width / fromInt(2), top + height / fromInt(2))
    Quadrants(
      topLeft = Rect(Interval(left, midX), Interval(top, midY)),
      topRight = Rect(Interval(midX + `1`, right), Interval(top, midY)),
      bottomLeft = Rect(Interval(left, midX), Interval(midY + `1`, bottom)),
      bottomRight = Rect(Interval(midX + `1`, right), Interval(midY + `1`, bottom)),
    )

  def growLeft(n: N): Rect[N] = copy(xRange = Interval(xRange.start - n, xRange.end))
  def growRight(n: N): Rect[N] = copy(xRange = Interval(xRange.start, xRange.end + n))
  def growTop(n: N): Rect[N] = copy(yRange = Interval(yRange.start - n, yRange.end))
  def growBottom(n: N): Rect[N] = copy(yRange = Interval(yRange.start, yRange.end + n))
  def expand(n: N): Rect[N] = growLeft(n).growTop(n).growRight(n).growBottom(n)

  def dropLeft(n: N): Rect[N] = growLeft(-n)
  def dropRight(n: N): Rect[N] = growRight(-n)
  def dropTop(n: N): Rect[N] = growTop(-n)
  def dropBottom(n: N): Rect[N] = growBottom(-n)
  def contract(n: N): Rect[N] = expand(-n)

  def encloses(rhs: Rect[N]): Boolean =
    if rhs.isEmpty then true
    else if isEmpty then false
    else
      left <= rhs.left && right >= rhs.right && top <= rhs.top && bottom >= rhs.bottom

  def overlaps(rhs: Rect[N]): Boolean =
    nonEmpty && rhs.nonEmpty && left <= rhs.right && right >= rhs.left && top <= rhs.bottom && bottom >= rhs.top
  def intersects(rhs: Rect[N]): Boolean = overlaps(rhs)

  def intersect(rhs: Rect[N]): Rect[N] =
    if isEmpty then this
    else if rhs.isEmpty then rhs
    else
      Rect(
        xRange = Interval(left max rhs.left, right min rhs.right),
        yRange = Interval(top max rhs.top, bottom min rhs.bottom),
      )

  def &(rhs: Rect[N]): Rect[N] = intersect(rhs)

  def clamp(coord: Point[N]): Point[N] = Point(x = coord.x max left min right, y = coord.y max top min bottom)

  def wrap(coord: Point[N]): Point[N] = Point(x = ((coord.x - left) +% width) + left, ((coord.y - top) +% height) + top)

  def show(coord2Char: Point[N] => Char): String =
    import scala.collection.mutable
    val builder = mutable.StringBuilder(size.toInt + height.toInt)
    this.foreach(coord =>
      builder.append(coord2Char(coord))
      if coord.col == right then builder.append('\n')
    )
    builder.toString

case class Quadrants[N: Integral](topLeft: Rect[N], topRight: Rect[N], bottomLeft: Rect[N], bottomRight: Rect[N]):
  def toSeq: Seq[Rect[N]] = Seq(topLeft, topRight, bottomLeft, bottomRight)
