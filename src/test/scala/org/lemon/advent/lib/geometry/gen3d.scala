package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Shrink.*

given Arbitrary[Point3[Int]] =
  Arbitrary(Gen.resultOf((x: Short, y: Short, z: Short) => Point3(x.toInt, y.toInt, z.toInt)))
given Shrink[Point3[Int]] = Shrink { case Point3(x, y, z) => shrink((x, y, z)).map((x, y, z) => Point3(x, y, z)) }

val smallPoint3: Gen[Point3[Int]] =
  val component = Gen.choose(-8, 8)
  Gen.zip(component, component, component).map((x, y, z) => Point3(x, y, z))

given Arbitrary[Cuboid[Int]] =
  val bound = Gen.choose(-6, 6)
  val interval = for a <- bound; b <- bound yield Interval(a min b, a max b)
  Arbitrary(Gen.zip(interval, interval, interval).map((x, y, z) => Cuboid(x, y, z)))

given Shrink[Cuboid[Int]] = Shrink { case Cuboid(x, y, z) =>
  shrink((x.start, x.end, y.start, y.end, z.start, z.end))
    .filter((x1, x2, y1, y2, z1, z2) => x1 <= x2 && y1 <= y2 && z1 <= z2)
    .map((x1, x2, y1, y2, z1, z2) => Cuboid(Interval(x1, x2), Interval(y1, y2), Interval(z1, z2)))
}

val emptyCuboid: Gen[Cuboid[Int]] =
  for
    cuboid <- Arbitrary.arbitrary[Cuboid[Int]]
    axis <- Gen.choose(0, 2)
    start <- Gen.choose(-6, 6)
    empty = Interval(start, start - 1)
  yield axis match
    case 0 => cuboid.copy(xRange = empty)
    case 1 => cuboid.copy(yRange = empty)
    case _ => cuboid.copy(zRange = empty)
