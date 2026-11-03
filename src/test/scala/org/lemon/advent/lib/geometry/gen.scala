package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.geometry.*
import org.scalacheck.{Arbitrary, Shrink, Gen}
import org.scalacheck.Shrink.*

given Arbitrary[Coord] = Arbitrary(Gen.resultOf(Coord.apply))
given Shrink[Coord] = Shrink { case Coord(x, y) => shrink((x, y)).map(Coord.apply) }

given Arbitrary[Point3[Int]] =
  Arbitrary(Gen.resultOf((x: Short, y: Short, z: Short) => Point3(x.toInt, y.toInt, z.toInt)))
given Shrink[Point3[Int]] = Shrink { case Point3(x, y, z) => shrink((x, y, z)).map((x, y, z) => Point3(x, y, z)) }

given Arbitrary[Direction] = Arbitrary(Gen.oneOf(Direction.values.toSeq))

given Arbitrary[Range] = Arbitrary(
  for
    i <- Gen.chooseNum(0, 100)
    j <- Gen.chooseNum(0, 100)
    if i != j
  yield math.min(i, j) to math.max(i, j)
)
given Shrink[Range] = Shrink { case range: Range =>
  shrink((range.start, range.end))
    .filter(_ != _)
    .map((start, end) => math.min(start, end) to math.max(start, end))
}

given Arbitrary[Area] = Arbitrary(Gen.resultOf((x: Range, y: Range) => Area(x, y)))
given Shrink[Area] = Shrink { case Area(xRange, yRange) =>
  shrink((xRange, yRange)).map(Area.apply.tupled)
}
