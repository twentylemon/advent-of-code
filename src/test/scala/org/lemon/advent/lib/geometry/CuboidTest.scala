package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class CuboidTest extends UnitTest:
  val emptyCuboid: Gen[Cuboid[Int]] =
    def emptied(range: Interval[Int]) = Gen.oneOf(range, Interval(range.start, range.start - 1))
    for
      cuboid <- Arbitrary.arbitrary[Cuboid[Int]]
      x <- emptied(cuboid.xRange)
      y <- emptied(cuboid.yRange)
      z <- emptied(cuboid.zRange)
      if x.isEmpty || y.isEmpty || z.isEmpty
    yield Cuboid(x, y, z)

  test("points cover cuboid") {
    check((cuboid: Cuboid[Int]) =>
      val points = cuboid.toSet
      Cuboid(points).toSet == points
    )
  }

  test("cuboid contains all enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.forall(cuboid.contains))
  }

  test("corners are the surface points the surface doesn't pass straight through") {
    check((cuboid: Cuboid[Int]) =>
      val surface = cuboid.faces.flatMap(_.toSeq).toSet
      cuboid.corners.toSet == surface.filterNot(p => Orientation.values.exists(o => surface(p + o) && surface(p - o)))
    )
  }

  test("number of enclosed points is equal to size") {
    check((cuboid: Cuboid[Int]) => cuboid.size == cuboid.iterator.size)
  }

  test("iterator goes by z, then y, then x") {
    check((cuboid: Cuboid[Int]) => cuboid.toSeq == cuboid.toSeq.sortBy(p => (p.z, p.y, p.x)))
  }

  test("surface area is the number of exposed unit faces") {
    check((cuboid: Cuboid[Int]) => cuboid.surfaceArea == cuboid.map(_.adjacent.count(!cuboid.contains(_))).sum)
  }

  test("surface area of an empty cuboid is 0") {
    check(forAll(emptyCuboid)(_.surfaceArea == 0))
  }

  test("leftFace contains height * depth points") {
    check((cuboid: Cuboid[Int]) => cuboid.leftFace.size == cuboid.height * cuboid.depth)
  }

  test("leftFace contains enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.leftFace.forall(cuboid.contains))
  }

  test("rightFace contains height * depth points") {
    check((cuboid: Cuboid[Int]) => cuboid.rightFace.size == cuboid.height * cuboid.depth)
  }

  test("rightFace contains enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.rightFace.forall(cuboid.contains))
  }

  test("topFace contains width * depth points") {
    check((cuboid: Cuboid[Int]) => cuboid.topFace.size == cuboid.width * cuboid.depth)
  }

  test("topFace contains enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.topFace.forall(cuboid.contains))
  }

  test("bottomFace contains width * depth points") {
    check((cuboid: Cuboid[Int]) => cuboid.bottomFace.size == cuboid.width * cuboid.depth)
  }

  test("bottomFace contains enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.bottomFace.forall(cuboid.contains))
  }

  test("frontFace contains width * height points") {
    check((cuboid: Cuboid[Int]) => cuboid.frontFace.size == cuboid.width * cuboid.height)
  }

  test("frontFace contains enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.frontFace.forall(cuboid.contains))
  }

  test("backFace contains width * height points") {
    check((cuboid: Cuboid[Int]) => cuboid.backFace.size == cuboid.width * cuboid.height)
  }

  test("backFace contains enclosed points") {
    check((cuboid: Cuboid[Int]) => cuboid.backFace.forall(cuboid.contains))
  }

  test("faces contain only points in cuboid") {
    check((cuboid: Cuboid[Int]) => cuboid.faces.forall(_.forall(cuboid.contains)))
  }

  test("face points are on the surface") {
    check((cuboid: Cuboid[Int]) => cuboid.faces.forall(_.forall(p => p.adjacent.exists(!cuboid.contains(_)))))
  }

  test("faces cover the surface") {
    check((cuboid: Cuboid[Int]) =>
      cuboid.filter(_.adjacent.exists(!cuboid.contains(_))).forall(p => cuboid.faces.exists(_.contains(p)))
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
      cuboid.octants.forall(o =>
        o.width == (if o.left == cuboid.left then (cuboid.width + 1) / 2 else cuboid.width / 2) &&
          o.height == (if o.top == cuboid.top then (cuboid.height + 1) / 2 else cuboid.height / 2) &&
          o.depth == (if o.front == cuboid.front then (cuboid.depth + 1) / 2 else cuboid.depth / 2)
      )
    )
  }

  test("octants are non-empty") {
    check((cuboid: Cuboid[Int]) => cuboid.octants.forall(_.nonEmpty))
  }

  test("an empty cuboid has no octants") {
    check(forAll(emptyCuboid)(_.octants.isEmpty))
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
      val disjoint =
        Cuboid(cuboid.right + 1 to cuboid.right + 2, cuboid.top to cuboid.bottom, cuboid.front to cuboid.back)
      !cuboid.overlaps(disjoint)
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
      val disjoint =
        Cuboid(cuboid.right + 1 to cuboid.right + 2, cuboid.top to cuboid.bottom, cuboid.front to cuboid.back)
      cuboid.intersect(disjoint).isEmpty
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

  test("diff with self is empty") {
    check((cuboid: Cuboid[Int]) => cuboid.diff(cuboid).isEmpty)
  }

  test("diff of disjoint cuboid is the original cuboid") {
    check((cuboid: Cuboid[Int]) =>
      val disjoint =
        Cuboid(cuboid.right + 1 to cuboid.right + 2, cuboid.top to cuboid.bottom, cuboid.front to cuboid.back)
      cuboid.diff(disjoint) == Seq(cuboid)
    )
  }

  test("diff is enclosed by original cuboid") {
    check((c1: Cuboid[Int], c2: Cuboid[Int]) => c1.diff(c2).forall(c1.encloses))
  }

  test("diff does not overlap the removed cuboid") {
    check((c1: Cuboid[Int], c2: Cuboid[Int]) => c1.diff(c2).forall(!_.overlaps(c2)))
  }

  test("diff cuboids do not overlap each other") {
    check((c1: Cuboid[Int], c2: Cuboid[Int]) => c1.diff(c2).pairs.forall(!_.overlaps(_)))
  }

  test("diff contains the points not in the removed cuboid") {
    check((c1: Cuboid[Int], c2: Cuboid[Int]) => c1.diff(c2).flatMap(_.toSeq).toSet == c1.filterNot(c2.contains).toSet)
  }

  test("diff of an empty cuboid is empty") {
    check(forAll(emptyCuboid, Arbitrary.arbitrary[Cuboid[Int]])((empty, cuboid) => empty.diff(cuboid).isEmpty))
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

  test("xy projects every point") {
    check((cuboid: Cuboid[Int]) => cuboid.xy.toSet == cuboid.map(_.xy).toSet)
  }

  test("xy size times depth is size") {
    check((cuboid: Cuboid[Int]) => cuboid.xy.size * cuboid.depth == cuboid.size)
  }

  test("xz projects every point") {
    check((cuboid: Cuboid[Int]) => cuboid.xz.toSet == cuboid.map(_.xz).toSet)
  }

  test("xz size times height is size") {
    check((cuboid: Cuboid[Int]) => cuboid.xz.size * cuboid.height == cuboid.size)
  }

  test("yz projects every point") {
    check((cuboid: Cuboid[Int]) => cuboid.yz.toSet == cuboid.map(_.yz).toSet)
  }

  test("yz size times width is size") {
    check((cuboid: Cuboid[Int]) => cuboid.yz.size * cuboid.width == cuboid.size)
  }
