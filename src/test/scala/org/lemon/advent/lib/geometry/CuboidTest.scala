package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class CuboidTest extends UnitTest:
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

  test("bounding cuboid is the smallest that encloses the points") {
    check(forAll(Gen.nonEmptyListOf(Arbitrary.arbitrary[Point3[Int]])) { (points: List[Point3[Int]]) =>
      val cuboid = Cuboid(points)
      points.forall(cuboid.contains) &&
      cuboid.topLeftFront == Point3(points.map(_.x).min, points.map(_.y).min, points.map(_.z).min) &&
      cuboid.bottomRightBack == Point3(points.map(_.x).max, points.map(_.y).max, points.map(_.z).max)
    })
  }

  test("size constructor has that size, starting at the origin") {
    val length = Gen.choose(1, Int.MaxValue)
    check(forAll(length, length, length) { (width: Int, height: Int, depth: Int) =>
      val cuboid = Cuboid(width, height, depth)
      cuboid.topLeftFront == Point3.origin[Int] && (cuboid.width, cuboid.height, cuboid.depth) == (width, height, depth)
    })
  }

  test("each named corner is the point its three steps leave the cuboid from") {
    val corners: Seq[(Cuboid[Int] => Point3[Int], Seq[Point3[Int] => Point3[Int]])] = Seq(
      (_.topLeftFront, Seq(_.up, _.left, _.out)),
      (_.topRightFront, Seq(_.up, _.right, _.out)),
      (_.bottomLeftFront, Seq(_.down, _.left, _.out)),
      (_.bottomRightFront, Seq(_.down, _.right, _.out)),
      (_.topLeftBack, Seq(_.up, _.left, _.in)),
      (_.topRightBack, Seq(_.up, _.right, _.in)),
      (_.bottomLeftBack, Seq(_.down, _.left, _.in)),
      (_.bottomRightBack, Seq(_.down, _.right, _.in)),
    )
    check((cuboid: Cuboid[Int]) =>
      corners.forall((corner, steps) =>
        val points = cuboid.iterator.filter(point => steps.forall(step => !cuboid.contains(step(point))))
        points.toSeq == Seq(corner(cuboid))
      )
    )
  }

  test("corners are every combination of the bounds") {
    check((cuboid: Cuboid[Int]) =>
      val combinations =
        for
          x <- Seq(cuboid.left, cuboid.right)
          y <- Seq(cuboid.top, cuboid.bottom)
          z <- Seq(cuboid.front, cuboid.back)
        yield Point3(x, y, z)
      cuboid.corners.size == 8 && cuboid.corners.toSet == combinations.toSet
    )
  }

  test("corners are contained, and bound every point") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.corners.forall(cuboid.contains) && Cuboid(cuboid.corners) == cuboid
    )
  }

  test("surface area is the number of exposed unit faces") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.surfaceArea == cuboid.iterator.map(_.adjacent.count(!cuboid.contains(_))).sum
    )
  }

  test("cuboid contains all iterated points") {
    check((cuboid: Cuboid[Int]) => cuboid.forall(cuboid.contains))
  }

  test("number of iterated points is the size") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.size == cuboid.size)
  }

  test("iterated points are distinct") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.toSeq.distinct.size == cuboid.size)
  }

  test("cuboid contains only the iterated points") {
    check((cuboid: Cuboid[Int]) =>
      val points = cuboid.iterator.toSet
      cuboid.expand(1).iterator.forall(point => cuboid.contains(point) == points.contains(point))
    )
  }

  test("each named face is the points its step leaves the cuboid from") {
    val faces: Seq[(Cuboid[Int] => Cuboid[Int], Point3[Int] => Point3[Int])] = Seq(
      (_.leftFace, _.left),
      (_.rightFace, _.right),
      (_.topFace, _.up),
      (_.bottomFace, _.down),
      (_.frontFace, _.out),
      (_.backFace, _.in),
    )
    check((cuboid: Cuboid[Int]) =>
      faces.forall((face, step) =>
        face(cuboid).iterator.toSet == cuboid.iterator.filterNot(point => cuboid.contains(step(point))).toSet
      )
    )
  }

  test("opposite faces project to the same rect as the cuboid") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.leftFace.yz == cuboid.yz && cuboid.rightFace.yz == cuboid.yz &&
        cuboid.topFace.xz == cuboid.xz && cuboid.bottomFace.xz == cuboid.xz &&
        cuboid.frontFace.xy == cuboid.xy && cuboid.backFace.xy == cuboid.xy
    )
  }

  test("faces are the points on the surface") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.faces.flatMap(_.iterator).toSet == cuboid.iterator.filter(_.adjacent.exists(!cuboid.contains(_))).toSet
    )
  }

  test("faces are enclosed, and one wide along their axis") {
    check((cuboid: Cuboid[Int]) =>
      val faces = cuboid.faces
      faces.size == 6 && faces.forall(cuboid.encloses) &&
      Seq(faces(0).width, faces(1).width, faces(2).height, faces(3).height, faces(4).depth, faces(5).depth).forall(
        _ == 1
      )
    )
  }

  test("face sizes sum to the surface area") {
    check((cuboid: Cuboid[Int]) => cuboid.faces.map(_.size).sum == cuboid.surfaceArea)
  }

  test("octants are disjoint") {
    check((cuboid: Cuboid[Int]) => cuboid.octants.pairs.forall(!_.overlaps(_)))
  }

  test("octants are at most 8, non-empty and enclosed") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.octants.size <= 8 && cuboid.octants.forall(octant => octant.nonEmpty && cuboid.encloses(octant))
    )
  }

  test("octant sizes sum to the size") {
    check((cuboid: Cuboid[Int]) => cuboid.octants.map(_.size).sum == cuboid.size)
  }

  test("octants split the cuboid in half, with any odd middle in the lower octants") {
    check((cuboid: Cuboid[Int]) =>
      val low = cuboid.octants.head
      low.topLeftFront == cuboid.topLeftFront &&
      (low.width, low.height, low.depth) == ((cuboid.width + 1) / 2, (cuboid.height + 1) / 2, (cuboid.depth + 1) / 2)
    )
  }

  test("octants of a point is the point") {
    check((point: Point3[Int]) => Cuboid(Seq(point)).octants == Seq(Cuboid(Seq(point))))
  }

  test("octants at the max value do not wrap around") {
    val corner = Cuboid(Seq(Point3(Int.MaxValue, Int.MaxValue, Int.MaxValue)))
    corner.octants shouldBe Seq(corner)
  }

  test("expand grows each side") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], Gen.choose(0, 10)) { (cuboid: Cuboid[Int], n: Int) =>
      val expanded = cuboid.expand(n)
      expanded.topLeftFront == cuboid.topLeftFront - Point3(n, n, n) &&
      expanded.bottomRightBack == cuboid.bottomRightBack + Point3(n, n, n)
    })
  }

  test("contract undoes expand") {
    check((cuboid: Cuboid[Int], n: Int) => cuboid.expand(n).contract(n) == cuboid)
  }

  test("translate moves every point") {
    val axis = Gen.choose(-1000, 1000)
    val offsets = Gen.zip(axis, axis, axis).map((x, y, z) => Point3(x, y, z))
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], offsets) { (cuboid: Cuboid[Int], offset: Point3[Int]) =>
      cuboid.translate(offset).iterator.toSet == cuboid.iterator.map(_ + offset).toSet
    })
  }

  test("translate by the opposite offset is the original") {
    check((cuboid: Cuboid[Int], offset: Point3[Int]) => cuboid.translate(offset).translate(-offset) == cuboid)
  }

  test("encloses when the intersect is the whole") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => a.encloses(b) == ((a & b) == b))
  }

  test("overlaps when the intersect is non-empty") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => a.overlaps(b) == (a & b).nonEmpty)
  }

  test("intersect contains the points in both") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => (a & b).iterator.toSet == a.iterator.filter(b.contains).toSet)
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

  test("diff size is the size outside the intersect") {
    check((a: Cuboid[Int], b: Cuboid[Int]) => (a -- b).map(_.size).sum == a.size - (a & b).size)
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
    pieces.map(_.size).sum shouldBe 15
    (edge -- edge) shouldBe empty
  }

  test("clamp lands in the cuboid") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) => cuboid.contains(cuboid.clamp(point)))
  }

  test("clamp keeps points in the cuboid") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.forall(point => cuboid.clamp(point) == point))
  }

  test("wrap lands in the cuboid") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) => cuboid.contains(cuboid.wrap(point)))
  }

  test("wrap keeps points in the cuboid") {
    check((cuboid: Cuboid[Int]) => cuboid.iterator.forall(point => cuboid.wrap(point) == point))
  }

  test("wrap undoes moving a point by whole sizes") {
    val gen =
      for
        cuboid <- Arbitrary.arbitrary[Cuboid[Int]]
        point <- Gen.oneOf(cuboid.iterator.toSeq)
        n <- Gen.choose(-1000, 1000)
      yield (cuboid, point, n)
    check(forAll(gen) { (cuboid, point, n) =>
      cuboid.wrap(point + Point3(cuboid.width, cuboid.height, cuboid.depth) * n) == point
    })
  }

  test("projections times the dropped length is the size") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.xy.size * cuboid.depth == cuboid.size &&
        cuboid.xz.size * cuboid.height == cuboid.size &&
        cuboid.yz.size * cuboid.width == cuboid.size
    )
  }

  test("empty cuboid has no points, size, or surface area") {
    check(forAll(emptyCuboid) { (cuboid: Cuboid[Int]) =>
      cuboid.isEmpty && cuboid.iterator.isEmpty && cuboid.size == 0 && cuboid.surfaceArea == 0
    })
  }

  test("empty cuboid contains nothing") {
    check(forAll(emptyCuboid, Arbitrary.arbitrary[Point3[Int]]) { (cuboid: Cuboid[Int], point: Point3[Int]) =>
      !cuboid.contains(point)
    })
  }

  test("faces of an empty cuboid throws") {
    check(forAll(emptyCuboid) { (cuboid: Cuboid[Int]) =>
      Prop.throws(classOf[UnsupportedOperationException])(cuboid.faces)
    })
  }

  test("empty cuboid has no octants") {
    check(forAll(emptyCuboid) { (cuboid: Cuboid[Int]) => cuboid.octants.isEmpty })
  }

  test("everything encloses an empty cuboid, and an empty cuboid encloses nothing") {
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], emptyCuboid) { (cuboid: Cuboid[Int], empty: Cuboid[Int]) =>
      cuboid.encloses(empty) && !empty.encloses(cuboid)
    })
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
