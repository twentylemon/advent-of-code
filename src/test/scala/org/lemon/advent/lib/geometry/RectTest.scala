package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class RectTest extends UnitTest:
  val emptyArea: Gen[Area] =
    def emptied(range: Interval[Int]) = Gen.oneOf(range, Interval(range.start, range.start - 1))
    for
      area <- arbitrary[Area]
      x <- emptied(area.xRange)
      y <- emptied(area.yRange)
      if x.isEmpty || y.isEmpty
    yield Rect(x, y)

  val growableArea: Gen[(Area, Int)] =
    for
      area <- arbitrary[Area]
      n <- Gen.chooseNum(0, Int.MaxValue - Seq(area.left, area.right, area.top, area.bottom).map(_.abs).max)
    yield (area, n)

  test("area contains all enclosed points") {
    check((area: Area) => area.forall(area.contains))
  }

  test("number of enclosed points is equal to size") {
    check((area: Area) => area.size == area.iterator.size)
  }

  test("iterator goes row by row from the top left") {
    check((area: Area) => area.toSeq == area.toSeq.sortBy(c => (c.y, c.x)))
  }

  test("perimeter is the number of exposed unit edges") {
    check((area: Area) => area.perimeter == area.map(_.adjacent.count(!area.contains(_))).sum)
  }

  test("perimeter of an empty area is 0") {
    check(forAll(emptyArea)(_.perimeter == 0))
  }

  test("corners are the boundary coords the boundary doesn't pass straight through") {
    check((area: Area) =>
      val boundary = area.boundary.toSet
      area.corners.toSet == boundary.filterNot(c => Direction.values.exists(d => boundary(c + d) && boundary(c - d)))
    )
  }

  test("each corner is the area's closest coord to that corner of the plane") {
    val (min, max) = (Int.MinValue, Int.MaxValue)
    val corner = Gen.oneOf(
      Seq[(Area => Coord, Coord)](
        (_.topLeft, Coord(min, min)),
        (_.topRight, Coord(max, min)),
        (_.bottomLeft, Coord(min, max)),
        (_.bottomRight, Coord(max, max)),
      )
    )
    check(forAll(arbitrary[Area], corner) { case (area, (at, far)) => at(area) == area.clamp(far) })
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

  test("row contains the coords in that row") {
    val gen = for area <- arbitrary[Area]; y <- Gen.oneOf(area.yRange) yield (area, y)
    check(forAll(gen) { (area, y) => area.row(y).toSeq == area.filter(_.y == y).toSeq })
  }

  test("row throws IllegalArgumentException outside the area") {
    check((area: Area, y: Int) =>
      !area.yRange.contains(y) ==> Prop.throws(classOf[IllegalArgumentException])(area.row(y))
    )
  }

  test("reverseRow is row in reverse") {
    val gen = for area <- arbitrary[Area]; y <- Gen.oneOf(area.yRange) yield (area, y)
    check(forAll(gen) { (area, y) => area.reverseRow(y).toSeq == area.row(y).toSeq.reverse })
  }

  test("reverseRow throws IllegalArgumentException outside the area") {
    check((area: Area, y: Int) =>
      !area.yRange.contains(y) ==> Prop.throws(classOf[IllegalArgumentException])(area.reverseRow(y))
    )
  }

  test("col contains the coords in that column") {
    val gen = for area <- arbitrary[Area]; x <- Gen.oneOf(area.xRange) yield (area, x)
    check(forAll(gen) { (area, x) => area.col(x).toSeq == area.filter(_.x == x).toSeq })
  }

  test("col throws IllegalArgumentException outside the area") {
    check((area: Area, x: Int) =>
      !area.xRange.contains(x) ==> Prop.throws(classOf[IllegalArgumentException])(area.col(x))
    )
  }

  test("reverseCol is col in reverse") {
    val gen = for area <- arbitrary[Area]; x <- Gen.oneOf(area.xRange) yield (area, x)
    check(forAll(gen) { (area, x) => area.reverseCol(x).toSeq == area.col(x).toSeq.reverse })
  }

  test("reverseCol throws IllegalArgumentException outside the area") {
    check((area: Area, x: Int) =>
      !area.xRange.contains(x) ==> Prop.throws(classOf[IllegalArgumentException])(area.reverseCol(x))
    )
  }

  test("boundary of an empty area is empty") {
    check(forAll(emptyArea)(_.boundary.isEmpty))
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

  test("boundary size is the number of edge coords") {
    check((area: Area) => area.boundary.size == area.count(_.adjacent.exists(!area.contains(_))))
  }

  test("upDiagonal throws IllegalArgumentException outside the area") {
    check((area: Area, coord: Coord) =>
      !area.contains(coord) ==> Prop.throws(classOf[IllegalArgumentException])(area.upDiagonal(coord))
    )
  }

  test("downDiagonal throws IllegalArgumentException outside the area") {
    check((area: Area, coord: Coord) =>
      !area.contains(coord) ==> Prop.throws(classOf[IllegalArgumentException])(area.downDiagonal(coord))
    )
  }

  test("map with coord keys cover area") {
    check((xRange: Range, yRange: Range) =>
      val grid = (for x <- xRange; y <- yRange yield Coord(x, y)).map(x => (x, 1)).toMap
      Area(grid).toSet == grid.keySet
    )
  }

  test("Area of lines covers every char") {
    val gen =
      for
        width <- Gen.choose(1, 20)
        height <- Gen.choose(1, 20)
        lines <- Gen.listOfN(height, Gen.stringOfN(width, Gen.alphaNumChar))
      yield lines
    check(forAll(gen) { lines =>
      Area(lines).toSet == Coord.gridToMap(lines.mkString("\n")).keySet && Area(lines).size == lines.map(_.length).sum
    })
  }

  test("Area of a grid covers every cell") {
    val gen =
      for
        width <- Gen.choose(1, 20)
        height <- Gen.choose(1, 20)
        grid <- Gen.listOfN(height, Gen.listOfN(width, arbitrary[Int]))
      yield grid
    check(forAll(gen) { grid => Area(grid).forall(grid.hasCoord) && Area(grid).size == grid.map(_.size).sum })
  }

  test("Area(width, height) starts at the origin") {
    check(forAll(Gen.chooseNum(1, Int.MaxValue), Gen.chooseNum(1, Int.MaxValue)) { (width, height) =>
      val area = Area(width, height)
      area.topLeft == Coord.origin && area.width == width && area.height == height
    })
  }

  test("rows includes all coords in same iteration order as overall area") {
    check((area: Area) => area.rows.flatMap(_.toSeq).sameElements(area))
  }

  test("cols includes all coords in same iteration order as transposed area") {
    check((area: Area) =>
      val transposed = Area(xRange = area.yRange, yRange = area.xRange)
      transposed.cols.flatMap(_.toSeq).sameElements(area.map(_.flip))
    )
  }

  test("upDiagonals contains all coords once") {
    check((area: Area) => area.upDiagonals.flatten.toSeq.sorted == area.toSeq.sorted)
  }

  test("upDiagonals step up and right") {
    check((area: Area) => area.upDiagonals.forall(_.sliding2.forall((a, b) => b == a.up.right)))
  }

  test("downDiagonals contains all coords once") {
    check((area: Area) => area.downDiagonals.flatten.toSeq.sorted == area.toSeq.sorted)
  }

  test("downDiagonals step down and right") {
    check((area: Area) => area.downDiagonals.forall(_.sliding2.forall((a, b) => b == a.down.right)))
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

  test("an empty area has no rectangles") {
    check(forAll(emptyArea, Gen.choose(-5, 5), Gen.choose(-5, 5)) { (area, x, y) => area.rectangles(x, y).isEmpty })
  }

  test("quadrants cover the area") {
    check((area: Area) => area.quadrants.flatMap(_.toSeq).toSet == area.toSet)
  }

  test("quadrants split the area in half, with any odd middle in the top or left quadrants") {
    check((area: Area) =>
      area.quadrants.forall(q =>
        q.width == (if q.left == area.left then (area.width + 1) / 2 else area.width / 2) &&
          q.height == (if q.top == area.top then (area.height + 1) / 2 else area.height / 2)
      )
    )
  }

  test("quadrants are non-empty") {
    check((area: Area) => area.quadrants.forall(_.nonEmpty))
  }

  test("an empty area has no quadrants") {
    check(forAll(emptyArea)(_.quadrants.isEmpty))
  }

  test("area encloses itself") {
    check((area: Area) => area `encloses` area)
  }

  test("area enclosed by larger area") {
    check((area: Area) => area.expand(1).encloses(area))
  }

  test("area not enclosed by smaller area") {
    check((area: Area) => !area.contract(1).encloses(area))
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
    check((area: Area) => !area.overlaps(Area(area.right + 1 to area.right + 2, area.top to area.bottom)))
  }

  test("empty area overlaps nothing") {
    check(forAll(emptyArea, arbitrary[Area])((empty, area) => !empty.overlaps(area) && !area.overlaps(empty)))
  }

  test("area overlaps when the intersection is non-empty") {
    check((a1: Area, a2: Area) => a1.overlaps(a2) == a1.intersect(a2).nonEmpty)
  }

  test("intersect with self is self") {
    check((area: Area) => area.intersect(area) == area)
  }

  test("intersect of disjoint areas is empty") {
    check((area: Area) => area.intersect(Area(area.right + 1 to area.right + 2, area.top to area.bottom)).isEmpty)
  }

  test("intersect is enclosed by both areas") {
    check((a1: Area, a2: Area) =>
      val inter = a1.intersect(a2)
      a1.encloses(inter) && a2.encloses(inter)
    )
  }

  test("intersect contains only points in both areas") {
    check((a1: Area, a2: Area) => a1.intersect(a2).forall(c => a1.contains(c) && a2.contains(c)))
  }

  test("diff with self is empty") {
    check((area: Area) => area.diff(area).isEmpty)
  }

  test("diff of disjoint area is the original area") {
    check((area: Area) => area.diff(Area(area.right + 1 to area.right + 2, area.top to area.bottom)) == Seq(area))
  }

  test("diff is enclosed by original area") {
    check((a1: Area, a2: Area) => a1.diff(a2).forall(a1.encloses))
  }

  test("diff does not overlap the removed area") {
    check((a1: Area, a2: Area) => a1.diff(a2).forall(!_.overlaps(a2)))
  }

  test("diff areas do not overlap each other") {
    check((a1: Area, a2: Area) => a1.diff(a2).pairs.forall(!_.overlaps(_)))
  }

  test("diff contains the points not in the removed area") {
    check((a1: Area, a2: Area) => a1.diff(a2).flatMap(_.toSeq).toSet == a1.filterNot(a2.contains).toSet)
  }

  test("diff of an empty area is empty") {
    check(forAll(emptyArea, arbitrary[Area])((empty, area) => empty.diff(area).isEmpty))
  }

  test("translate moves every point") {
    val axis = Gen.choose(-1000, 1000)
    val offsets = Gen.zip(axis, axis).map((x, y) => Coord(x, y))
    check(forAll(arbitrary[Area], offsets) { (area, offset) =>
      area.translate(offset).toSet == area.map(_ + offset).toSet
    })
  }

  test("translate by the opposite offset is the original") {
    check((area: Area, offset: Coord) => area.translate(offset).translate(-offset) == area)
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
    check(forAll(growableArea) { (area, n) => area.growLeft(n) `encloses` area })
  }

  test("growRight encloses original area") {
    check(forAll(growableArea) { (area, n) => area.growRight(n) `encloses` area })
  }

  test("growTop encloses original area") {
    check(forAll(growableArea) { (area, n) => area.growTop(n) `encloses` area })
  }

  test("growBottom encloses original area") {
    check(forAll(growableArea) { (area, n) => area.growBottom(n) `encloses` area })
  }

  test("expand encloses original area") {
    check(forAll(growableArea) { (area, n) => area.expand(n) `encloses` area })
  }

  test("expand and contract are inverse") {
    check((area: Area, n: Int) => area.expand(n).contract(n) == area)
  }

  test("contract is enclosed by original area") {
    check((area: Area) => area `encloses` area.contract(1))
  }

  test("clamp returns a coord in the area") {
    check((area: Area, coord: Coord) => area.contains(area.clamp(coord)))
  }

  test("clamp of coord inside area is identity") {
    check((area: Area) => area.forall(c => area.clamp(c) == c))
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
