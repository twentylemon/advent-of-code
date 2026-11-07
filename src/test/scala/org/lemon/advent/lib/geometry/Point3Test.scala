package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class Point3Test extends UnitTest:

  test("+ and - are opposites") {
    check((a: Point3[Int], b: Point3[Int]) => a + b - b == a)
  }

  test("unary - is * -1") {
    check((a: Point3[Int]) => -a == a * -1)
  }

  test("left and right are opposites") {
    check((a: Point3[Int]) => a.left.right == a)
  }

  test("shiftLeft(n) and shiftRight(n) are opposites") {
    check((a: Point3[Int], n: Int) => a.shiftLeft(n).shiftRight(n) == a)
  }

  test("up and down are opposites") {
    check((a: Point3[Int]) => a.up.down == a)
  }

  test("shiftUp(n) and shiftDown(n) are opposites") {
    check((a: Point3[Int], n: Int) => a.shiftUp(n).shiftDown(n) == a)
  }

  test("in and out are opposites") {
    check((a: Point3[Int]) => a.in.out == a)
  }

  test("shiftIn(n) and shiftOut(n) are opposites") {
    check((a: Point3[Int], n: Int) => a.shiftIn(n).shiftOut(n) == a)
  }

  test("+ unitUp is up") {
    check((a: Point3[Int]) => a + Point3.unitUp[Int] == a.up)
  }

  test("+ unitDown is down") {
    check((a: Point3[Int]) => a + Point3.unitDown[Int] == a.down)
  }

  test("+ unitLeft is left") {
    check((a: Point3[Int]) => a + Point3.unitLeft[Int] == a.left)
  }

  test("+ unitRight is right") {
    check((a: Point3[Int]) => a + Point3.unitRight[Int] == a.right)
  }

  test("+ unitIn is in") {
    check((a: Point3[Int]) => a + Point3.unitIn[Int] == a.in)
  }

  test("+ unitOut is out") {
    check((a: Point3[Int]) => a + Point3.unitOut[Int] == a.out)
  }

  test("- unitUp is down") {
    check((a: Point3[Int]) => a - Point3.unitUp[Int] == a.down)
  }

  test("- unitDown is up") {
    check((a: Point3[Int]) => a - Point3.unitDown[Int] == a.up)
  }

  test("- unitLeft is right") {
    check((a: Point3[Int]) => a - Point3.unitLeft[Int] == a.right)
  }

  test("- unitRight is left") {
    check((a: Point3[Int]) => a - Point3.unitRight[Int] == a.left)
  }

  test("- unitIn is out") {
    check((a: Point3[Int]) => a - Point3.unitIn[Int] == a.out)
  }

  test("- unitOut is in") {
    check((a: Point3[Int]) => a - Point3.unitOut[Int] == a.in)
  }

  test("move(orientation) is + facing") {
    check((a: Point3[Int], o: Orientation) => a.move(o) == a + o.facing)
  }

  test("shift(orientation, n) is + facing * n") {
    check((a: Point3[Int], o: Orientation, n: Int) => a.shift(o, n) == a + o.facing * n)
  }

  test("+ orientation is move(orientation)") {
    check((a: Point3[Int], o: Orientation) => a + o == a.move(o))
  }

  test("- orientation is move(orientation.turnAround)") {
    check((a: Point3[Int], o: Orientation) => a - o == a.move(o.turnAround))
  }

  test("adjacent has 6 distinct points") {
    check((a: Point3[Int]) => a.adjacent.distinct.size == 6)
  }

  test("adjacent points are at manhattan distance 1") {
    check((a: Point3[Int]) => a.adjacent.forall(_.manhattan(a) == 1))
  }

  test("surrounding has 26 distinct points") {
    check((a: Point3[Int]) => a.surrounding.distinct.size == 26)
  }

  test("surrounding points are at chessboard distance 1") {
    check((a: Point3[Int]) => a.surrounding.forall(_.chessboard(a) == 1))
  }

  test("surrounding contains adjacent") {
    check((a: Point3[Int]) => a.adjacent.forall(a.surrounding.contains))
  }

  test("manhattan is symmetric") {
    check((a: Point3[Int], b: Point3[Int]) => a.manhattan(b) == b.manhattan(a))
  }

  test("manhattan obeys the triangle inequality") {
    check((a: Point3[Int], b: Point3[Int], c: Point3[Int]) =>
      val (p, q, r) = (a.to[BigInt], b.to[BigInt], c.to[BigInt])
      p.manhattan(r) <= p.manhattan(q) + q.manhattan(r)
    )
  }

  test("chessboard is at most manhattan") {
    check((a: Point3[Int], b: Point3[Int]) =>
      val (p, q) = (a.to[BigInt], b.to[BigInt])
      p.chessboard(q) <= p.manhattan(q)
    )
  }

  test("euclidean is the dot product of the difference") {
    check((a: Point3[Int], b: Point3[Int]) => a.euclidean(b) == (a - b).dot(a - b))
  }

  test("bounding box defines opposing corners") {
    check((a: Point3[Int], b: Point3[Int]) =>
      val cuboid = a.bounding(b)
      Seq(
        Set(cuboid.topLeftFront, cuboid.bottomRightBack),
        Set(cuboid.topRightFront, cuboid.bottomLeftBack),
        Set(cuboid.bottomLeftFront, cuboid.topRightBack),
        Set(cuboid.bottomRightFront, cuboid.topLeftBack),
      ).contains(Set(a, b))
    )
  }

  test("dot is symmetric") {
    check((a: Point3[Int], b: Point3[Int]) => a.dot(b) == b.dot(a))
  }

  test("cross is orthogonal to both vectors") {
    check((a: Point3[Int], b: Point3[Int]) =>
      val cross = a.cross(b)
      cross.dot(a) == 0 && cross.dot(b) == 0
    )
  }

  test("cross is antisymmetric") {
    check((a: Point3[Int], b: Point3[Int]) => a.cross(b) == -b.cross(a))
  }

  test("cross follows the right hand rule") {
    Point3.unitRight[Int].cross(Point3.unitDown[Int]) shouldBe Point3.unitIn[Int]
    Point3.unitDown[Int].cross(Point3.unitIn[Int]) shouldBe Point3.unitRight[Int]
    Point3.unitIn[Int].cross(Point3.unitRight[Int]) shouldBe Point3.unitDown[Int]
  }

  test("reduce is idempotent") {
    check((a: Point3[Int]) => a.reduce.reduce == a.reduce)
  }

  test("reduce keeps the direction") {
    check((a: Point3[Int]) =>
      val reduced = a.reduce
      reduced.cross(a) == Point3.origin[Int] &&
      reduced.x.sign == a.x.sign && reduced.y.sign == a.y.sign && reduced.z.sign == a.z.sign
    )
  }

  test("reduce leaves coprime components") {
    check((a: Point3[Int]) => a != Point3.origin[Int] ==> (a.reduce.x.gcd(a.reduce.y).gcd(a.reduce.z) == 1))
  }

  test("reduce of the origin is the origin") {
    Point3.origin[Int].reduce shouldBe Point3.origin[Int]
  }

  test("reduce of axis aligned vectors are unit vectors") {
    check((n: Int) =>
      (n != 0 && n != Int.MinValue) ==>
        Seq(Point3.unitRight[Int], Point3.unitDown[Int], Point3.unitIn[Int]).forall(unit =>
          (unit * n).reduce == unit * n.sign
        )
    )
  }

  test("reduce at the min value keeps the direction") {
    Point3(Int.MinValue, Int.MinValue, Int.MinValue).reduce shouldBe Point3(-1, -1, -1)
    Point3(Int.MinValue, 0, Int.MinValue).reduce shouldBe Point3(-1, 0, -1)
    Point3(Int.MinValue, 0, 0).reduce shouldBe Point3(-1, 0, 0)
  }

  test("projections send the dropped axis to the origin") {
    check((n: Int) =>
      (Point3.unitIn[Int] * n).xy == Coord.origin &&
        (Point3.unitDown[Int] * n).xz == Coord.origin &&
        (Point3.unitRight[Int] * n).yz == Coord.origin
    )
  }

  test("projections are linear") {
    val projections = Gen.oneOf[Point3[Int] => Point[Int]](_.xy, _.xz, _.yz)
    check(forAll(Arbitrary.arbitrary[Point3[Int]], Arbitrary.arbitrary[Point3[Int]], projections) {
      (a, b, project) => project(a + b) == project(a) + project(b)
    })
  }

  test("the cross products of the projections are the components of the cross product") {
    check((a: Point3[Int], b: Point3[Int]) =>
      val cross = a.cross(b)
      a.yz.cross(b.yz) == cross.x && a.xz.cross(b.xz) == -cross.y && a.xy.cross(b.xy) == cross.z
    )
  }

  test("to[Int] is identity") {
    check((a: Point3[Int]) => a.to[Int] == a)
  }

  test("to[Long] and back is identity") {
    check((a: Point3[Int]) => a.to[Long].to[Int] == a)
  }

  test("to[Long] preserves coordinates") {
    check((a: Point3[Int]) => a.to[Long] == Point3[Long](a.x.toLong, a.y.toLong, a.z.toLong))
  }
