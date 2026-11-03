package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class Point3Test extends UnitTest:

  test("+ and - are opposites") {
    check((a: Coord3, b: Coord3) => a + b - b == a)
  }

  test("unary - is * -1") {
    check((a: Coord3) => -a == a * -1)
  }

  test("tuples convert to points") {
    val point: Coord3 = (1, 2, 3)
    point shouldBe Coord3(1, 2, 3)
  }

  test("ordering compares x, then y, then z") {
    check((a: Coord3, b: Coord3) =>
      Ordering[Coord3].compare(a, b).sign == Ordering[(Int, Int, Int)].compare((a.x, a.y, a.z), (b.x, b.y, b.z)).sign
    )
  }

  test("adjacent has 6 distinct points at manhattan distance 1") {
    check((a: Coord3) => a.adjacent.distinct.size == 6 && a.adjacent.forall(_.manhattan(a) == 1))
  }

  test("surrounding has 26 distinct points at chessboard distance 1") {
    check((a: Coord3) => a.surrounding.distinct.size == 26 && a.surrounding.forall(_.chessboard(a) == 1))
  }

  test("surrounding contains adjacent") {
    check((a: Coord3) => a.adjacent.forall(a.surrounding.contains))
  }

  test("manhattan is symmetric") {
    check((a: Coord3, b: Coord3) => a.manhattan(b) == b.manhattan(a))
  }

  test("manhattan obeys the triangle inequality") {
    check((a: Coord3, b: Coord3, c: Coord3) => a.manhattan(c) <= a.manhattan(b) + b.manhattan(c))
  }

  test("chessboard is at most manhattan") {
    check((a: Coord3, b: Coord3) => a.chessboard(b) <= a.manhattan(b))
  }

  test("euclidean is the dot product of the difference") {
    check((a: Coord3, b: Coord3) => a.to[Long].euclidean(b.to[Long]) == (a - b).to[Long].dot((a - b).to[Long]))
  }

  test("dot is symmetric") {
    check((a: Coord3, b: Coord3) => a.dot(b) == b.dot(a))
  }

  test("cross is orthogonal to both vectors") {
    check((a: Coord3, b: Coord3) =>
      val (lhs, rhs) = (a.to[Long], b.to[Long])
      val cross = lhs.cross(rhs)
      cross.dot(lhs) == 0 && cross.dot(rhs) == 0
    )
  }

  test("cross is antisymmetric") {
    check((a: Coord3, b: Coord3) => a.cross(b) == -b.cross(a))
  }

  test("cross follows the right hand rule") {
    Coord3.unitX.cross(Coord3.unitY) shouldBe Coord3.unitZ
    Coord3.unitY.cross(Coord3.unitZ) shouldBe Coord3.unitX
    Coord3.unitZ.cross(Coord3.unitX) shouldBe Coord3.unitY
  }

  test("reduce is idempotent") {
    check((a: Coord3) => a.reduce.reduce == a.reduce)
  }

  test("reduce keeps the direction") {
    check((a: Coord3) =>
      val reduced = a.reduce
      reduced.cross(a) == Coord3.origin &&
      reduced.x.sign == a.x.sign && reduced.y.sign == a.y.sign && reduced.z.sign == a.z.sign
    )
  }

  test("reduce leaves coprime components") {
    check((a: Coord3) => a != Coord3.origin ==> (a.reduce.x.gcd(a.reduce.y).gcd(a.reduce.z) == 1))
  }

  test("reduce of the origin is the origin") {
    Coord3.origin.reduce shouldBe Coord3.origin
  }

  test("reduce of axis aligned vectors are unit vectors") {
    check((n: Short) =>
      n > 0 ==> Seq(Coord3.unitX, Coord3.unitY, Coord3.unitZ).forall(unit => (unit * n).reduce == unit)
    )
  }

  test("reduce at the min value keeps the direction") {
    Coord3(Int.MinValue, Int.MinValue, Int.MinValue).reduce shouldBe Coord3(-1, -1, -1)
    Coord3(Int.MinValue, 0, Int.MinValue).reduce shouldBe Coord3(-1, 0, -1)
    Coord3(Int.MinValue, 0, 0).reduce shouldBe Coord3(-1, 0, 0)
  }

  test("there are 24 distinct rotations") {
    Coord3(1, 2, 3).rotations.distinct.size shouldBe 24
  }

  test("the first rotation is the identity") {
    check((a: Coord3) => a.rotations.head == a)
  }

  test("rotations preserve the manhattan distance from the origin") {
    check((a: Coord3) => a.rotations.forall(_.manhattan(Coord3.origin) == a.manhattan(Coord3.origin)))
  }

  test("rotations are linear") {
    check((a: Coord3, b: Coord3) => (a + b).rotations == a.rotations.lazyZip(b.rotations).map(_ + _))
  }

  test("rotations preserve cross products, so none are reflections") {
    check((a: Coord3, b: Coord3) =>
      val (lhs, rhs) = (a.to[Long], b.to[Long])
      lhs.cross(rhs).rotations == lhs.rotations.lazyZip(rhs.rotations).map(_ `cross` _)
    )
  }

  test("rotations of a rotation are the same rotations") {
    check((a: Coord3) => a.rotations.forall(_.rotations.toSet == a.rotations.toSet))
  }

  test("bounding contains both points") {
    check((a: Coord3, b: Coord3) => a.bounding(b).contains(a) && a.bounding(b).contains(b))
  }

  test("bounding is the smallest cuboid with both points") {
    check((a: Coord3, b: Coord3) => a.bounding(b) == Cuboid(Seq(a, b)))
  }

  test("projections drop an axis") {
    check((a: Coord3) => a.xy == Coord(a.x, a.y) && a.xz == Coord(a.x, a.z) && a.yz == Coord(a.y, a.z))
  }

  test("to[Long] and back is identity") {
    check((a: Coord3) => a.to[Long].to[Int] == a)
  }
