package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.+%

import scala.collection.immutable.WrappedString

object Area:
  def apply(grid: Map[Coord, _]): Area = apply(grid.keySet)

  def apply(coords: Iterable[Coord]): Area =
    val (xs, ys) = (coords.map(_.x), coords.map(_.y))
    Area(xRange = xs.min to xs.max, yRange = ys.min to ys.max)

  def apply(grid: Seq[Seq[_]]): Area = Area(yRange = grid.indices, xRange = grid.head.indices)

  def apply(lines: Seq[String])(using String => WrappedString): Area =
    Area(yRange = lines.indices, xRange = lines.head.indices)

  def apply(left: Int, right: Int, top: Int, bottom: Int): Area = Area(left to right, top to bottom)
  def apply(width: Int, height: Int): Area = apply(0, width - 1, 0, height - 1)

  given Conversion[Area, Iterator[Coord]] =
    (area: Area) => for y <- area.yRange.iterator; x <- area.xRange.iterator yield (x, y)

  extension [T](seq: Seq[String])(using String => WrappedString)
    def apply(area: Area): Seq[Seq[Char]] = seq.slice(area.top, area.bottom + 1).map(_.slice(area.left, area.right + 1))

  extension [T](seq: Seq[Seq[T]])
    def apply(area: Area): Seq[Seq[T]] = seq.slice(area.top, area.bottom + 1).map(_.slice(area.left, area.right + 1))

/** A 2d rectangular area.
  * @param xRange the x bounds of the rectangle
  * @param yRange the y bounds of the rectangle
  */
case class Area(xRange: Range, yRange: Range):
  def left = xRange.min
  def right = xRange.max
  def top = yRange.min
  def bottom = yRange.max

  def topLeft: Coord = (left, top)
  def bottomRight: Coord = (right, bottom)
  def topRight: Coord = (right, top)
  def bottomLeft: Coord = (left, bottom)

  def width = xRange.size
  def height = yRange.size

  def size = width * height

  def contains(coord: Coord): Boolean = xRange.contains(coord.x) && yRange.contains(coord.y)
  def apply(coord: Coord): Boolean = contains(coord)

  def col(x: Int): Iterator[Coord] =
    assert(xRange.contains(x), s"$this  does not contain column  $x")
    for y <- yRange.iterator yield (x, y)

  def leftCol: Iterator[Coord] = col(left)
  def rightCol: Iterator[Coord] = col(right)

  def row(y: Int): Iterator[Coord] =
    assert(yRange.contains(y), s"$this  does not contain row  $y")
    for x <- xRange.iterator yield (x, y)

  def topRow: Iterator[Coord] = row(top)
  def bottomRow: Iterator[Coord] = row(bottom)

  def upDiagonal(startAt: Coord): Iterator[Coord] =
    assert(contains(startAt), s"$this  does not contain startAt  $startAt")
    Iterator.iterate(startAt)(_.up.right).takeWhile(contains)

  def downDiagonal(startAt: Coord): Iterator[Coord] =
    assert(contains(startAt), s"$this  does not contain startAt  $startAt")
    Iterator.iterate(startAt)(_.down.right).takeWhile(contains)

  def rows: Iterator[Iterator[Coord]] = yRange.iterator.map(row)
  def cols: Iterator[Iterator[Coord]] = xRange.iterator.map(col)
  def upDiagonals: Iterator[Iterator[Coord]] =
    xRange.iterator.map(x => upDiagonal((x, bottom))) ++ yRange.reverseIterator.drop(1).map(y => upDiagonal((left, y)))
  def downDiagonals: Iterator[Iterator[Coord]] =
    xRange.iterator.map(x => downDiagonal((x, top))) ++ yRange.iterator.drop(1).map(y => downDiagonal((left, y)))
  def diagonals: Iterator[Iterator[Coord]] = upDiagonals ++ downDiagonals

  /** Iterates over _all_ (width x height) areas within this area.
    * Areas returned will overlap each other, it's _every_ sub-area.
    * @param width the sub-area width
    * @param height the sub-aea height
    * @return iterator of all sub-areas of the given size in this area
    */
  def rectangles(width: Int, height: Int): Iterator[Area] =
    for
      x <- xRange.sliding(width)
      y <- yRange.sliding(height)
    yield Area(x.head to x.last, y.head to y.last)

  /** Returns the four quadrants of this area. If the area has an odd width or height,
    * the middle row or column will be in the top or left quadrants.
    * @return the four quadrant areas of this area
    */
  def quadrants: Quadrants =
    val (midX, midY) = (left + width / 2, top + height / 2)
    Quadrants(
      topLeft = Area(left to midX, top to midY),
      topRight = Area(midX + 1 to right, top to midY),
      bottomLeft = Area(left to midX, midY + 1 to bottom),
      bottomRight = Area(midX + 1 to right, midY + 1 to bottom),
    )

  def dropLeft(n: Int): Area = Area(xRange.drop(n), yRange)
  def dropRight(n: Int): Area = Area(xRange.dropRight(n), yRange)
  def dropTop(n: Int): Area = Area(xRange, yRange.drop(n))
  def dropBottom(n: Int): Area = Area(xRange, yRange.dropRight(n))

  def encloses(area: Area): Boolean =
    left <= area.left && right >= area.right && top <= area.top && bottom >= area.bottom

  def clamp(coord: Coord): Coord = Coord(x = coord.x max left min right, y = coord.y max top min bottom)

  def show(coord2Char: Coord => Char): String =
    import scala.collection.mutable
    val builder = mutable.StringBuilder(size + height)
    this.foreach(coord =>
      builder.append(coord2Char(coord))
      if coord.col == right then builder.append('\n')
    )
    builder.toString

case class Quadrants(topLeft: Area, topRight: Area, bottomLeft: Area, bottomRight: Area):
  def seq: Seq[Area] = Seq(topLeft, topRight, bottomLeft, bottomRight)
