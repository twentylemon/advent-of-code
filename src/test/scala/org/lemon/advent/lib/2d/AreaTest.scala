package org.lemon.advent.lib.`2d`

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.scalacheck.Prop.*
import org.scalacheck.*

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

  test("reverseRow contains same points as row") {
    check((area: Area) => area.reverseRow(area.top).toSet == area.topRow.toSet)
  }

  test("reverseCol contains same points as col") {
    check((area: Area) => area.reverseCol(area.left).toSet == area.leftCol.toSet)
  }

  test("boundary contains only coords in area") {
    check((area: Area) => area.boundary.forall(area.contains))
  }

  test("boundary has no duplicates") {
    check((area: Area) =>
      val b = area.boundary.toSeq
      b.distinct == b
    )
  }

  test("boundary coords are on the edge") {
    check((area: Area) => area.boundary.forall(c => c.adjacent.exists(!area.contains(_))))
  }

  test("boundary size is perimeter") {
    check((area: Area) => area.boundary.size == Region(area.toSet).perimeter.size)
  }

  test("map with coord keys cover area") {
    check((xRange: Range, yRange: Range) =>
      val grid = (for x <- xRange; y <- yRange yield Coord(x, y)).map(x => (x, 1)).toMap
      Area(grid).toSet == grid.keySet
    )
  }

  test("rows includes all coords in same iteration order as overall area") {
    check((area: Area) => area.rows.flatMap(identity).sameElements(area))
  }

  test("cols includes all coords in same iteration order as transposed area") {
    check((area: Area) =>
      val transposed = Area(xRange = area.yRange, yRange = area.xRange)
      transposed.cols.flatMap(identity).sameElements(area.map(_.flip))
    )
  }

  test("upDiagonals contains all coords once") {
    check((area: Area) => area.upDiagonals.flatMap(identity).toSeq.diff(area.toSeq).isEmpty)
  }

  test("downDiagonals contains all coords once") {
    check((area: Area) => area.downDiagonals.flatMap(identity).toSeq.diff(area.toSeq).isEmpty)
  }

  test("rectangles are all correct dimension") {
    check((area: Area) =>
      (area.width > 4 && area.height > 4) ==> {
        val (x, y) = (area.width - 3, area.height - 3)
        area.rectangles(x, y).forall(r => r.width == x && r.height == y)
      }
    )
  }

  test("rectangles are all enclosed by area") {
    check((area: Area) =>
      (area.width > 4 && area.height > 4) ==> {
        val (x, y) = (area.width - 3, area.height - 3)
        area.rectangles(x, y).forall(area.encloses)
      }
    )
  }

  test("no duplicate rectangles") {
    check((area: Area) =>
      (area.width > 4 && area.height > 4) ==> {
        val (x, y) = (area.width - 3, area.height - 3)
        val rect = area.rectangles(x, y).toSeq
        rect.distinct == rect
      }
    )
  }

  test("quadrants cover the area") {
    check((area: Area) => area.quadrants.toSeq.flatMap(_.toSeq).toSet == area.toSet)
  }

  test("area encloses itself") {
    check((area: Area) => area `encloses` area)
  }

  test("area enclosed by larger area") {
    check((area: Area) => area.growLeft(1).growRight(1).growTop(1).growBottom(1).encloses(area))
  }

  test("area not enclosed by smaller area") {
    check((area: Area) =>
      (area.width > 2 && area.height > 2) ==>
        !area.dropLeft(1).dropRight(1).dropTop(1).dropBottom(1).encloses(area)
    )
  }

  test("area not enclosed by disjoint area") {
    check((area: Area) => !area.encloses(Area(area.right + 1 to area.right + 2, area.top - 2 to area.top - 1)))
  }

  test("area overlaps itself") {
    check((area: Area) => area `overlaps` area)
  }

  test("area overlaps adjacent area") {
    check((area: Area) => area `overlaps` area.growLeft(1).growRight(1).growTop(1).growBottom(1))
  }

  test("area does not overlap disjoint area") {
    check((area: Area) => !area.overlaps(Area(area.right + 1 to area.right + 2, area.yRange)))
  }

  test("intersect with self is self") {
    check((area: Area) => area.intersect(area).contains(area))
  }

  test("intersect of disjoint areas is empty") {
    check((area: Area) => area.intersect(Area(area.right + 1 to area.right + 2, area.yRange)).isEmpty)
  }

  test("intersect is enclosed by both areas") {
    check((a1: Area, a2: Area) =>
      a1.intersect(a2).forall(inter => a1.encloses(inter) && a2.encloses(inter))
    )
  }

  test("intersect contains only points in both areas") {
    check((a1: Area, a2: Area) =>
      a1.intersect(a2).forall(inter => inter.forall(c => a1.contains(c) && a2.contains(c)))
    )
  }

  test("growLeft and dropLeft are inverse") {
    check((area: Area, n: Int) => area.growLeft(n).dropLeft(n) == area)
  }

  test("growRight and dropRight are inverse") {
    check((area: Area, n: Int) => area.growRight(n).dropRight(n) == area)
  }

  test("growTop and dropTop are inverse") {
    check((area: Area, n: Int) => area.growTop(n).dropTop(n) == area)
  }

  test("growBottom and dropBottom are inverse") {
    check((area: Area, n: Int) => area.growBottom(n).dropBottom(n) == area)
  }

  test("growLeft encloses original area") {
    check((area: Area, n: Int) => (n > 0 && n < Int.MaxValue - 1000) ==> (area.growLeft(n) `encloses` area))
  }

  test("growRight encloses original area") {
    check((area: Area, n: Int) => (n > 0 && n < Int.MaxValue - 1000) ==> (area.growRight(n) `encloses` area))
  }

  test("growTop encloses original area") {
    check((area: Area, n: Int) => (n > 0 && n < Int.MaxValue - 1000) ==> (area.growTop(n) `encloses` area))
  }

  test("growBottom encloses original area") {
    check((area: Area, n: Int) => (n > 0 && n < Int.MaxValue - 1000) ==> (area.growBottom(n) `encloses` area))
  }

  test("expand encloses original area") {
    check((area: Area, n: Int) => (n > 0 && n < Int.MaxValue - 1000) ==> (area.expand(n) `encloses` area))
  }

  test("expand and contract are inverse") {
    check((area: Area, n: Int) => area.expand(n).contract(n) == area)
  }

  test("contract is enclosed by original area") {
    check((area: Area) => (area.width > 2 && area.height > 2) ==> (area `encloses` area.contract(1)))
  }

  test("clamp returns a coord in the area") {
    check((area: Area, coord: Coord) => area.contains(area.clamp(coord)))
  }

  test("wrap returns a coord in the area") {
    check((area: Area, coord: Coord) => area.contains(area.wrap(coord)))
  }

  test("wrap of coord inside area is identity") {
    check((area: Area) => area.forall(c => area.wrap(c) == c))
  }

  test("wrap x coordinate wraps toroidally") {
    check((area: Area, coord: Coord) =>
      ((area.wrap(coord).x - area.left) +% area.width) == ((coord.x - area.left) +% area.width)
    )
  }

  test("wrap y coordinate wraps toroidally") {
    check((area: Area, coord: Coord) =>
      ((area.wrap(coord).y - area.top) +% area.height) == ((coord.y - area.top) +% area.height)
    )
  }
