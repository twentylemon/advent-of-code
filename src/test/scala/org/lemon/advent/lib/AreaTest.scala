package org.lemon.advent.lib

import org.lemon.advent._
import org.lemon.advent.lib.Coord2._
import org.scalacheck.Prop._
import org.scalacheck._

class AreaTest extends UnitTest:

  test("area contains all enclosed points") {
    check((area: Area) => area.forall(area.contains))
  }

  test("number of enclosed points is equal to size") {
    check((area: Area) => area.size == area.iterator.size)
  }

  test("topRow contains width points") {
    check((area: Area) => area.topRow.size == area.width)
  }

  test("topRow contains enclosed points") {
    check((area: Area) => area.topRow.forall(area.contains))
  }

  test("bottomRow contains width points") {
    check((area: Area) => area.bottomRow.size == area.width)
  }

  test("bottomRow contains enclosed points") {
    check((area: Area) => area.bottomRow.forall(area.contains))
  }

  test("leftCol contains height points") {
    check((area: Area) => area.leftCol.size == area.height)
  }

  test("leftCol contains enclosed points") {
    check((area: Area) => area.leftCol.forall(area.contains))
  }

  test("rightCol contains height points") {
    check((area: Area) => area.rightCol.size == area.height)
  }

  test("rightCol contains enclosed points") {
    check((area: Area) => area.rightCol.forall(area.contains))
  }

  test("map with coord keys cover area") {
    check((xRange: Range, yRange: Range) =>
      val grid = (for x <- xRange; y <- yRange yield Coord(x, y)).map(x => (x, 1)).toMap
      Area(grid).toSet == grid.keySet
    )
  }
