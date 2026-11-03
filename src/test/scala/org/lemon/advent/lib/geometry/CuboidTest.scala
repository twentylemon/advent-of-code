package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class CuboidTest extends UnitTest:

  test("cuboid contains all iterated points") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.forall(cuboid.contains))
  }

  test("number of iterated points is the volume") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.size == cuboid.volume)
  }

  test("iterated points are distinct") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.toSeq.distinct.size == cuboid.volume)
  }

  test("cuboid contains only the iterated points") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], smallPoint3) { (cuboid: Cuboid[Int], point: Point3[Int]) =>
      cuboid.contains(point) == cuboid.iterator.contains(point)
    })
  }

  test("converts to an iterator of its points") {
    val points: Iterator[Point3[Int]] = Cuboid(2, 3, 4)
    points.size shouldBe 24
  }

  test("size constructor starts at the origin") {
    Cuboid(2, 3, 4) shouldBe Cuboid(0 to 1, 0 to 2, 0 to 3)
  }

  test("bounding cuboid is the smallest that encloses the points") {
    check(forAll(Gen.nonEmptyListOf(smallPoint3)) { (points: List[Point3[Int]]) =>
      val cuboid = Cuboid(points)
      points.forall(cuboid.contains) &&
      cuboid.min == Point3(points.map(_.x).min, points.map(_.y).min, points.map(_.z).min) &&
      cuboid.max == Point3(points.map(_.x).max, points.map(_.y).max, points.map(_.z).max)
    })
  }

  test("corners are contained, and bound every point") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.corners.forall(cuboid.contains) && Cuboid(cuboid.corners) == cuboid
    )
  }

  test("intersect contains the points in both") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => (a & b).iterator.toSet == a.iterator.filter(b.contains).toSet)
  }

  test("overlaps when the intersect is non-empty") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => a.overlaps(b) == (a & b).nonEmpty)
  }

  test("encloses when the intersect is the whole") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => a.encloses(b) == ((a & b) == b))
  }

  test("diff pieces are disjoint") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => (a -- b).pairs.forall(!_.overlaps(_)))
  }

  test("diff pieces are non-empty, enclosed, and miss the removed cuboid") {
    check((a: Cuboid[Int], b: Cuboid[Int]) =>
      val pieces = a -- b
      pieces.size <= 6 && pieces.forall(piece => piece.nonEmpty && a.encloses(piece) && !piece.overlaps(b))
    )
  }

  test("diff volume is the volume outside the intersect") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => (a -- b).map(_.volume).sum == a.volume - (a & b).volume)
  }

  test("diff points are the set difference") {
    check((a: Cuboid[Int], b: Cuboid[Int]) =>
      (a -- b).flatMap(_.iterator).toSet == a.iterator.filterNot(b.contains).toSet
    )
  }

  test("diff of a disjoint cuboid is the whole") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => !a.overlaps(b) ==> ((a -- b) == Seq(a)))
  }

  test("diff of itself is empty") {
    check((cuboid: Cuboid[Int]) => (cuboid -- cuboid).isEmpty)
  }

  test("diff at the edges of Int does not wrap around") {
    val edge = Cuboid(
      Interval(Int.MinValue, Int.MinValue + 2),
      Interval(Int.MaxValue - 2, Int.MaxValue),
      Interval(0, 2),
    )
    val corner = Cuboid(
      Interval(Int.MinValue, Int.MinValue + 1),
      Interval(Int.MaxValue - 1, Int.MaxValue),
      Interval(0, 2),
    )
    val pieces = edge -- corner
    pieces.forall(edge.encloses) shouldBe true
    pieces.map(_.volume).sum shouldBe 15
    (edge -- edge) shouldBe empty
  }

  test("octants are disjoint") {
    check((cuboid: Cuboid[Int]) => cuboid.octants.pairs.forall(!_.overlaps(_)))
  }

  test("octants are at most 8, non-empty and enclosed") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.octants.size <= 8 && cuboid.octants.forall(octant => octant.nonEmpty && cuboid.encloses(octant))
    )
  }

  test("octant volumes sum to the volume") {
    check((cuboid: Cuboid[Int]) => cuboid.octants.map(_.volume).sum == cuboid.volume)
  }

  test("octants put the middle slice in the lower half") {
    Cuboid(3, 1, 1).octants shouldBe Seq(Cuboid(0 to 1, 0 to 0, 0 to 0), Cuboid(2 to 2, 0 to 0, 0 to 0))
  }

  test("octants of a point is the point") {
    check((point: Point3[Int]) => Cuboid(Seq(point)).octants == Seq(Cuboid(Seq(point))))
  }

  test("octants at the max value do not wrap around") {
    val corner = Cuboid(Seq(Point3(Int.MaxValue, Int.MaxValue, Int.MaxValue)))
    corner.octants shouldBe Seq(corner)
  }

  test("contract undoes expand") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], Gen.choose(0, 10)) { (cuboid: Cuboid[Int], n: Int) =>
      cuboid.expand(n).contract(n) == cuboid
    })
  }

  test("expand grows each side") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], Gen.choose(0, 10)) { (cuboid: Cuboid[Int], n: Int) =>
      val expanded = cuboid.expand(n)
      expanded.min == cuboid.min - Point3(n, n, n) && expanded.max == cuboid.max + Point3(n, n, n)
    })
  }

  test("+ moves every point") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], smallPoint3) { (cuboid: Cuboid[Int], offset: Point3[Int]) =>
      (cuboid + offset).iterator.toSet == cuboid.iterator.map(_ + offset).toSet
    })
  }

  test("+ and - are opposites") {
    check((cuboid: Cuboid[Int], offset: Point3[Int]) => cuboid + offset - offset == cuboid)
  }

  test("clamp lands in the cuboid") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) => cuboid.contains(cuboid.clamp(point)))
  }

  test("clamp keeps points in the cuboid") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.forall(point => cuboid.clamp(point) == point))
  }

  test("surface area is the number of exposed unit faces") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.surfaceArea == cuboid.iterator.map(_.adjacent.count(!cuboid.contains(_))).sum
    )
  }

  test("faces times their depth is the volume") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.xy.size * cuboid.depth == cuboid.volume &&
        cuboid.xz.size * cuboid.height == cuboid.volume &&
        cuboid.yz.size * cuboid.width == cuboid.volume
    )
  }

  test("empty cuboid contains nothing") {
    check(forAll(emptyCuboid, smallPoint3) { (cuboid: Cuboid[Int], point: Point3[Int]) => !cuboid.contains(point) })
  }

  test("empty cuboid has no points, volume, or surface area") {
    check(forAll(emptyCuboid) { (cuboid: Cuboid[Int]) =>
      cuboid.isEmpty && cuboid.iterator.isEmpty && cuboid.volume == 0 && cuboid.surfaceArea == 0
    })
  }

  test("empty cuboid has no octants") {
    check(forAll(emptyCuboid) { (cuboid: Cuboid[Int]) => cuboid.octants.isEmpty })
  }

  test("diff of an empty cuboid is empty") {
    check(forAll(emptyCuboid, Arbitrary.arbitrary[Cuboid[Int]]) { (empty: Cuboid[Int], cuboid: Cuboid[Int]) =>
      (empty -- cuboid).isEmpty
    })
  }

  test("diff by an empty cuboid is the whole") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], emptyCuboid) { (cuboid: Cuboid[Int], empty: Cuboid[Int]) =>
      (cuboid -- empty) == Seq(cuboid)
    })
  }

  test("everything encloses an empty cuboid, and an empty cuboid encloses nothing") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], emptyCuboid) { (cuboid: Cuboid[Int], empty: Cuboid[Int]) =>
      cuboid.encloses(empty) && !empty.encloses(cuboid)
    })
  }
