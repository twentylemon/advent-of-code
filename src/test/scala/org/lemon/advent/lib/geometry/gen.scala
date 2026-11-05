package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*
import org.scalacheck.{Arbitrary, Shrink, Gen}
import org.scalacheck.Shrink.*

given Arbitrary[Coord] = Arbitrary(Gen.resultOf(Coord.apply))
given Shrink[Coord] = Shrink { case Coord(x, y) => shrink((x, y)).map(Coord.apply) }

given Arbitrary[Point3[Int]] = Arbitrary(Gen.resultOf(Point3.apply[Int]))
given Shrink[Point3[Int]] = Shrink { case Point3(x, y, z) => shrink((x, y, z)).map(Point3.apply[Int]) }

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

given Arbitrary[Cuboid[Int]] =
  val bound = Gen.choose(-12, 12)
  val interval = for a <- bound; b <- bound yield Interval(a min b, a max b)
  Arbitrary(Gen.zip(interval, interval, interval).map((x, y, z) => Cuboid(x, y, z)))

given Shrink[Cuboid[Int]] = Shrink { case Cuboid(x, y, z) =>
  shrink((x.start, x.end, y.start, y.end, z.start, z.end))
    .filter((x1, x2, y1, y2, z1, z2) => x1 <= x2 && y1 <= y2 && z1 <= z2)
    .map((x1, x2, y1, y2, z1, z2) => Cuboid(Interval(x1, x2), Interval(y1, y2), Interval(z1, z2)))
}
