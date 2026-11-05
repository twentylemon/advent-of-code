package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class CuboidTest extends UnitTest:
  val emptyCuboid: Gen[Cuboid[Int]] =
    for
      x <- Gen.choose(-100, 100)
      y <- Gen.choose(-100, 100)
      z <- Gen.choose(-100, 100)
    yield Cuboid(Interval(x, x - 1), Interval(y, y - 1), Interval(z, z - 1))

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

  test("cuboid contains all enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.forall(cuboid.contains))
  }

  test("number of enclosed points is equal to size") {
    check((cuboid: Cuboid[Int]) => cuboid.size == cuboid.iterator.size)
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

  test("octants cover the cuboid") {
    check((cuboid: Cuboid[Int]) => cuboid.octants.flatMap(_.toSeq).toSet == cuboid.toSet)
  }

  test("octants split the cuboid in half, with any odd middle in the lower octants") {
    check((cuboid: Cuboid[Int]) =>
      val (w, h, d) = (cuboid.width, cuboid.height, cuboid.depth)
      (w > 1 && h > 1 && d > 1) ==> {
        val (low, high) = (cuboid.octants.head, cuboid.octants.last)
        (low.width, low.height, low.depth) == ((w + 1) / 2, (h + 1) / 2, (d + 1) / 2) &&
        (high.width, high.height, high.depth) == (w / 2, h / 2, d / 2)
      }
    )
  }

  test("expand encloses original cuboid") {
    check((cuboid: Cuboid[Int], n: Int) => (n > 0 && n < Int.MaxValue - 1000) ==> (cuboid.expand(n) `encloses` cuboid))
  }

  test("expand and contract are inverse") {
    check((cuboid: Cuboid[Int], n: Int) => cuboid.expand(n).contract(n) == cuboid)
  }

  test("contract is enclosed by original cuboid") {
    check((cuboid: Cuboid[Int]) =>
      (cuboid.width > 2 && cuboid.height > 2 && cuboid.depth > 2) ==> (cuboid `encloses` cuboid.contract(1))
    )
  }

  test("translate moves every point") {
    val axis = Gen.choose(-1000, 1000)
    val offsets = Gen.zip(axis, axis, axis).map((x, y, z) => Point3(x, y, z))
    check(forAll(Arbitrary.arbitrary[Cuboid[Int]], offsets) { (cuboid: Cuboid[Int], offset: Point3[Int]) =>
      cuboid.translate(offset).toSet == cuboid.map(_ + offset).toSet
    })
  }

  test("translate by the opposite offset is the original") {
    check((cuboid: Cuboid[Int], offset: Point3[Int]) => cuboid.translate(offset).translate(-offset) == cuboid)
  }

  test("cuboid encloses itself") {
    check((cuboid: Cuboid[Int]) => cuboid `encloses` cuboid)
  }

  test("cuboid enclosed by larger cuboid") {
    check((cuboid: Cuboid[Int]) => cuboid.expand(1).encloses(cuboid))
  }

  test("cuboid not enclosed by smaller cuboid") {
    check((cuboid: Cuboid[Int]) =>
      (cuboid.width > 2 && cuboid.height > 2 && cuboid.depth > 2) ==> !cuboid.contract(1).encloses(cuboid)
    )
  }

  test("cuboid not enclosed by disjoint cuboid") {
    check((cuboid: Cuboid[Int]) =>
      val disjoint = Cuboid(
        cuboid.right + 1 to cuboid.right + 2,
        cuboid.top - 2 to cuboid.top - 1,
        cuboid.front - 2 to cuboid.front - 1,
      )
      !cuboid.encloses(disjoint)
    )
  }

  test("cuboid overlaps itself") {
    check((cuboid: Cuboid[Int]) => cuboid `overlaps` cuboid)
  }

  test("cuboid overlaps adjacent cuboid") {
    check((cuboid: Cuboid[Int]) => cuboid `overlaps` cuboid.expand(1))
  }

  test("cuboid does not overlap disjoint cuboid") {
    check((cuboid: Cuboid[Int]) =>
      !cuboid.overlaps(Cuboid(Interval(cuboid.right + 1, cuboid.right + 2), cuboid.yRange, cuboid.zRange))
    )
  }

  test("empty cuboid overlaps nothing") {
    check(forAll(emptyCuboid, Arbitrary.arbitrary[Cuboid[Int]]) { (empty: Cuboid[Int], cuboid: Cuboid[Int]) =>
      !empty.overlaps(cuboid) && !cuboid.overlaps(empty)
    })
  }

  test("cuboid overlaps when the intersection is non-empty") {
    check((c1: Cuboid[Int], c2: Cuboid[Int]) => c1.overlaps(c2) == c1.intersect(c2).nonEmpty)
  }

  test("intersect with self is self") {
    check((cuboid: Cuboid[Int]) => cuboid.intersect(cuboid) == cuboid)
  }

  test("intersect of disjoint cuboids is empty") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.intersect(Cuboid(Interval(cuboid.right + 1, cuboid.right + 2), cuboid.yRange, cuboid.zRange)).isEmpty
    )
  }

  test("intersect is enclosed by both cuboids") {
    check((c1: Cuboid[Int], c2: Cuboid[Int]) =>
      val inter = c1.intersect(c2)
      c1.encloses(inter) && c2.encloses(inter)
    )
  }

  test("intersect contains only points in both cuboids") {
    check((c1: Cuboid[Int], c2: Cuboid[Int]) => c1.intersect(c2).forall(p => c1.contains(p) && c2.contains(p)))
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

  test("clamp returns a point in the cuboid") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) => cuboid.contains(cuboid.clamp(point)))
  }

  test("wrap returns a point in the cuboid") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) => cuboid.contains(cuboid.wrap(point)))
  }

  test("wrap of point inside cuboid is identity") {
    check((cuboid: Cuboid[Int]) => cuboid.forall(p => cuboid.wrap(p) == p))
  }

  test("wrap x coordinate wraps toroidally") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) =>
      ((cuboid.wrap(point).x - cuboid.left) +% cuboid.width) == ((point.x - cuboid.left) +% cuboid.width)
    )
  }

  test("wrap y coordinate wraps toroidally") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) =>
      ((cuboid.wrap(point).y - cuboid.top) +% cuboid.height) == ((point.y - cuboid.top) +% cuboid.height)
    )
  }

  test("wrap z coordinate wraps toroidally") {
    check((cuboid: Cuboid[Int], point: Point3[Int]) =>
      ((cuboid.wrap(point).z - cuboid.front) +% cuboid.depth) == ((point.z - cuboid.front) +% cuboid.depth)
    )
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

  test("faces of an empty cuboid throws") {
    check(forAll(emptyCuboid) { (cuboid: Cuboid[Int]) =>
      Prop.throws(classOf[UnsupportedOperationException])(cuboid.faces)
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
