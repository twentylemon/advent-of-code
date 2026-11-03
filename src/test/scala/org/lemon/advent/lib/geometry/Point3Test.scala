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

  test("up and down are opposites") {
    check((a: Point3[Int]) => a.up.down == a)
  }

  test("in and out are opposites") {
    check((a: Point3[Int]) => a.in.out == a)
  }

  test("each step moves along its axis") {
    val origin = Point3.origin[Int]
    Seq(origin.left, origin.right, origin.up, origin.down, origin.in, origin.out) shouldBe Seq(
      Point3(-1, 0, 0),
      Point3(1, 0, 0),
      Point3(0, -1, 0),
      Point3(0, 1, 0),
      Point3(0, 0, 1),
      Point3(0, 0, -1),
    )
  }

  test("adjacent has 6 distinct points at manhattan distance 1") {
    check((a: Point3[Int]) => a.adjacent.distinct.size == 6 && a.adjacent.forall(_.manhattan(a) == 1))
  }

  test("surrounding has 26 distinct points at chessboard distance 1") {
    check((a: Point3[Int]) => a.surrounding.distinct.size == 26 && a.surrounding.forall(_.chessboard(a) == 1))
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
    Point3.unitX[Int].cross(Point3.unitY[Int]) shouldBe Point3.unitZ[Int]
    Point3.unitY[Int].cross(Point3.unitZ[Int]) shouldBe Point3.unitX[Int]
    Point3.unitZ[Int].cross(Point3.unitX[Int]) shouldBe Point3.unitY[Int]
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
        Seq(Point3.unitX[Int], Point3.unitY[Int], Point3.unitZ[Int]).forall(unit => (unit * n).reduce == unit * n.sign)
    )
  }

  test("reduce at the min value keeps the direction") {
    Point3(Int.MinValue, Int.MinValue, Int.MinValue).reduce shouldBe Point3(-1, -1, -1)
    Point3(Int.MinValue, 0, Int.MinValue).reduce shouldBe Point3(-1, 0, -1)
    Point3(Int.MinValue, 0, 0).reduce shouldBe Point3(-1, 0, 0)
  }

  test("there are 24 distinct rotations") {
    Point3(1, 2, 3).rotations.distinct.size shouldBe 24
  }

  test("the first rotation is the identity") {
    check((a: Point3[Int]) => a.rotations.head == a)
  }

  test("rotations preserve the manhattan distance from the origin") {
    check((a: Point3[Int]) => a.rotations.forall(_.manhattan(Point3.origin[Int]) == a.manhattan(Point3.origin[Int])))
  }

  test("rotations are linear") {
    check((a: Point3[Int], b: Point3[Int]) => (a + b).rotations == a.rotations.lazyZip(b.rotations).map(_ + _))
  }

  test("rotations preserve cross products, so none are reflections") {
    check((a: Point3[Int], b: Point3[Int]) => a.cross(b).rotations == a.rotations.lazyZip(b.rotations).map(_ `cross` _))
  }

  test("rotations of a rotation are the same rotations") {
    check((a: Point3[Int]) => a.rotations.forall(_.rotations.toSet == a.rotations.toSet))
  }

  test("projections drop an axis") {
    check((a: Point3[Int]) => a.xy == Coord(a.x, a.y) && a.xz == Coord(a.x, a.z) && a.yz == Coord(a.y, a.z))
  }

  test("to[Long] and back is identity") {
    check((a: Point3[Int]) => a.to[Long].to[Int] == a)
  }
