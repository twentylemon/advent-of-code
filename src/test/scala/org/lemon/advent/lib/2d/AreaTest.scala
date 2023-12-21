package org.lemon.advent.lib.`2d`

import org.lemon.advent._
import org.lemon.advent.lib.`2d`._
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

  test("area encloses itself") {
    check((area: Area) => area encloses area)
  }

  test("area enclosed by larger area") {
    check((area: Area) => Area(area.left - 1 to area.right + 1, area.top - 1 to area.bottom + 1).encloses(area))
  }

  test("area not enclosed by smaller area") {
    check((area: Area) =>
      (area.width > 2 && area.height > 2) ==>
        !Area(area.left + 1 to area.right - 1, area.top + 1 to area.bottom - 1).encloses(area)
    )
  }

  test("area not enclosed by disjoint area") {
    check((area: Area) => !area.encloses(Area(area.right + 1 to area.right + 2, area.top - 2 to area.top - 1)))
  }

  test("clamp returns a coord in the area") {
    check((area: Area, coord: Coord) => area.contains(area.clamp(coord)))
  }
