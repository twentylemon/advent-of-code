package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.`2d`._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

given Arbitrary[Coord] = Arbitrary(Gen.resultOf(Coord.apply))

given Arbitrary[Range] = Arbitrary(
  for
    i <- Gen.chooseNum(0, 100)
    j <- Gen.chooseNum(0, 100)
  yield math.min(i, j) to math.max(i, j)
)
given Arbitrary[Area] = Arbitrary(Gen.resultOf((x: Range, y: Range) => Area(x, y)))
