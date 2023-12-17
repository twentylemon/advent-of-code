package org.lemon.advent.lib

import org.scalacheck.Gen
import org.scalacheck.Gen.Choose
import org.scalacheck.Arbitrary

given Arbitrary[Coord2.Coord] = Arbitrary(Gen.resultOf(Coord2.Coord.apply))

given Arbitrary[Range] = Arbitrary(
  for
    i <- Gen.chooseNum(0, 100)
    j <- Gen.chooseNum(0, 100)
  yield math.min(i, j) to math.max(i, j)
)
given Arbitrary[Area] = Arbitrary(Gen.resultOf((x: Range, y: Range) => Area(x, y)))
