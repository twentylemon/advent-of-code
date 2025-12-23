package org.lemon.advent.lib.`2d`

import org.lemon.advent.*
import org.lemon.advent.lib.`2d`.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class SeqExtensionsTest extends UnitTest:

  given Arbitrary[Seq[Coord]] = Arbitrary(Gen.chooseNum(3, 10).flatMap(Gen.listOfN(_, Arbitrary.arbitrary[Coord])))

  def geometricArea(area: Area) = (area.right - area.left) * (area.bottom - area.top)

  test("shoelaceArea of rectangle matches geometric area") {
    check((area: Area) => area.corners.shoelaceArea == geometricArea(area))
  }

  test("shoelaceArea of right triangle is half base times height") {
    check((area: Area) =>
      val triangle = Seq(area.topLeft, area.topRight, area.bottomLeft)
      triangle.shoelaceArea == geometricArea(area) / 2
    )
  }

  test("shoelaceArea scales by k^2 when coordinates scale by k") {
    given Arbitrary[Int] = Arbitrary(Gen.chooseNum(1, 10))
    given Arbitrary[Seq[Coord]] = Arbitrary(for
      w <- Gen.chooseNum(2, 100)
      h <- Gen.chooseNum(2, 100)
      cutW <- Gen.chooseNum(1, w - 1)
      cutH <- Gen.chooseNum(1, h - 1)
    yield Seq(Coord(0, 0), Coord(w, 0), Coord(w, h - cutH), Coord(w - cutW, h - cutH), Coord(w - cutW, h), Coord(0, h)))
    check((polygon: Seq[Coord], k: Int) =>
      polygon.map(c => Coord(c.x * k, c.y * k)).shoelaceArea == polygon.shoelaceArea * k * k
    )
  }

  test("shoelaceArea of arbitrary polygon is invariant to translation") {
    check((polygon: Seq[Coord], shift: Coord) =>
      polygon.shoelaceArea == polygon.map(_ + shift).shoelaceArea
    )
  }

  test("shoelaceArea of arbitrary polygon is invariant to winding order") {
    check((polygon: Seq[Coord]) => polygon.shoelaceArea == polygon.reverse.shoelaceArea)
  }

  test("shoelaceArea of arbitrary polygon is invariant to starting vertex") {
    check((polygon: Seq[Coord], rotation: Int) =>
      val n = (rotation % polygon.size).abs
      polygon.shoelaceArea == (polygon.drop(n) ++ polygon.take(n)).shoelaceArea
    )
  }
