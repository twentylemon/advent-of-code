package org.lemon.advent.lib.geometry

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Shrink.*

given Arbitrary[Point3[Int]] =
  Arbitrary(Gen.resultOf((x: Short, y: Short, z: Short) => Point3(x.toInt, y.toInt, z.toInt)))
given Shrink[Point3[Int]] = Shrink { case Point3(x, y, z) => shrink((x, y, z)).map((x, y, z) => Point3(x, y, z)) }
