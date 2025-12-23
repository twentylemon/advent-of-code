package org.lemon.advent.lib.`2d`

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.Coord.*
import org.lemon.advent.lib.`2d`.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class CoordTest extends UnitTest:

  test("left and right are opposites") {
    check((coord: Coord) => coord == coord.left.right)
  }

  test("shiftLeft(n) and shiftRight(n) are opposites") {
    check((coord: Coord, n: Int) => coord == coord.shiftLeft(n).shiftRight(n))
  }

  test("up and down are opposites") {
    check((coord: Coord) => coord == coord.up.down)
  }

  test("shiftUp(n) and shiftDown(n) are opposites") {
    check((coord: Coord, n: Int) => coord == coord.shiftUp(n).shiftDown(n))
  }

  test("+ unitUp is up") {
    check((coord: Coord) => coord + unitUp == coord.up)
  }

  test("+ unitDown is down") {
    check((coord: Coord) => coord + unitDown == coord.down)
  }

  test("+ unitLeft is left") {
    check((coord: Coord) => coord + unitLeft == coord.left)
  }

  test("+ unitRight is right") {
    check((coord: Coord) => coord + unitRight == coord.right)
  }

  test("- unitUp is down") {
    check((coord: Coord) => coord - unitUp == coord.down)
  }

  test("- unitDown is up") {
    check((coord: Coord) => coord - unitDown == coord.up)
  }

  test("- unitLeft is right") {
    check((coord: Coord) => coord - unitLeft == coord.right)
  }

  test("- unitRight is left") {
    check((coord: Coord) => coord - unitRight == coord.left)
  }

  test("move(Up) is up") {
    check((coord: Coord) => coord.move(Direction.Up) == coord.up)
  }

  test("move(Down) is down") {
    check((coord: Coord) => coord.move(Direction.Down) == coord.down)
  }

  test("move(Left) is left") {
    check((coord: Coord) => coord.move(Direction.Left) == coord.left)
  }

  test("move(Right) is right") {
    check((coord: Coord) => coord.move(Direction.Right) == coord.right)
  }

  test("shift(Up, n) is shiftUp(n)") {
    check((coord: Coord, n: Int) => coord.shift(Direction.Up, n) == coord.shiftUp(n))
  }

  test("shift(Down, n) is shiftUp(n)") {
    check((coord: Coord, n: Int) => coord.shift(Direction.Down, n) == coord.shiftDown(n))
  }

  test("shift(Left, n) is shiftUp(n)") {
    check((coord: Coord, n: Int) => coord.shift(Direction.Left, n) == coord.shiftLeft(n))
  }

  test("shift(Right, n) is shiftUp(n)") {
    check((coord: Coord, n: Int) => coord.shift(Direction.Right, n) == coord.shiftRight(n))
  }

  test("walk nth element is shift by n") {
    given Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 100))
    check((coord: Coord, dir: Direction, n: Int) => coord.walk(dir).nth(n) == coord.shift(dir, n))
  }

  test("walk(step) first element is self") {
    check((coord: Coord, step: Coord) => coord.walk(step).next() == coord)
  }

  test("walk(step) nth element is coord + step * n") {
    given Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 100))
    check((coord: Coord, step: Coord, n: Int) => coord.walk(step).nth(n) == coord + step * n)
  }

  test("walk(step) with zero steps is always coord") {
    given Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 100))
    check((coord: Coord, n: Int) => coord.walk(Coord.origin).nth(n) == coord)
  }

  test("left is left direction") {
    check((coord: Coord) => coord.directionTo(coord.left) == Some(Direction.Left))
  }

  test("right is right direction") {
    check((coord: Coord) => coord.directionTo(coord.right) == Some(Direction.Right))
  }

  test("up is up direction") {
    check((coord: Coord) => coord.directionTo(coord.up) == Some(Direction.Up))
  }

  test("down is down direction") {
    check((coord: Coord) => coord.directionTo(coord.down) == Some(Direction.Down))
  }

  test("direction to self is None") {
    check((coord: Coord) => coord.directionTo(coord) == None)
  }

  test("direction off line is None") {
    check((coord: Coord) => coord.directionTo(coord.up.left) == None)
  }

  test("manhattan distance of adjacent is 1") {
    check((coord: Coord) => coord.adjacent.forall(_.manhattan(coord) == 1))
  }

  test("manhattan distance of diagonal is 2") {
    check((coord: Coord) =>
      Seq(coord.up.left, coord.up.right, coord.down.left, coord.down.right).forall(_.manhattan(coord) == 2)
    )
  }

  test("manhattan is symmetric") {
    check((c1: Coord, c2: Coord) => c1.manhattan(c2) == c2.manhattan(c1))
  }

  test("chessboard distance to self is 0") {
    check((coord: Coord) => coord.chessboard(coord) == 0)
  }

  test("chessboard distance of surrounding is 1") {
    check((coord: Coord) => coord.surrounding.forall(_.chessboard(coord) == 1))
  }

  test("chessboard is symmetric") {
    check((c1: Coord, c2: Coord) => c1.chessboard(c2) == c2.chessboard(c1))
  }

  test("euclidean distance to self is 0") {
    check((coord: Coord) => coord.euclidean(coord) == 0)
  }

  test("euclidean distance of adjacent is 1") {
    check((coord: Coord) => coord.adjacent.forall(_.euclidean(coord) == 1))
  }

  test("euclidean distance of diagonal is 2") {
    check((coord: Coord) =>
      Seq(coord.up.left, coord.up.right, coord.down.left, coord.down.right).forall(_.euclidean(coord) == 2)
    )
  }

  test("euclidean is symmetric") {
    check((c1: Coord, c2: Coord) => c1.euclidean(c2) == c2.euclidean(c1))
  }

  test("surrounding contains all adjacent") {
    check((coord: Coord) => coord.adjacent.toSet.subsetOf(coord.surrounding.toSet))
  }

  test("bounding box defines opposing corners") {
    check((c1: Coord, c2: Coord) =>
      val area = c1.bounding(c2)
      val coords = Set(c1, c2)
      coords == Set(area.topLeft, area.bottomRight) || coords == Set(area.topRight, area.bottomLeft)
    )
  }

  test("within distance 0 is just the coord") {
    given Arbitrary[(Coord, Coord) => Int] = Arbitrary(Gen.oneOf(
      Gen.const((a: Coord, b: Coord) => a.manhattan(b)),
      Gen.const((a: Coord, b: Coord) => a.chessboard(b))
    ))
    check((coord: Coord, metric: (Coord, Coord) => Int) => coord.within(0, metric).toSeq == Seq(coord))
  }

  test("within contains the coord") {
    given Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 10))
    given Arbitrary[(Coord, Coord) => Int] = Arbitrary(Gen.oneOf(
      Gen.const((a: Coord, b: Coord) => a.manhattan(b)),
      Gen.const((a: Coord, b: Coord) => a.chessboard(b))
    ))
    check((coord: Coord, d: Int, metric: (Coord, Coord) => Int) => coord.within(d, metric).contains(coord))
  }

  test("within distance d has correct bounds") {
    given Arbitrary[Int] = Arbitrary(Gen.chooseNum(1, 10))
    given Arbitrary[(Coord, Coord) => Int] = Arbitrary(Gen.oneOf(
      Gen.const((a: Coord, b: Coord) => a.manhattan(b)),
      Gen.const((a: Coord, b: Coord) => a.chessboard(b))
    ))
    check((coord: Coord, d: Int, metric: (Coord, Coord) => Int) =>
      coord.within(d, metric).forall(c => metric(coord, c) <= d)
    )
  }

  test("rotateRight four times is identity") {
    check((coord: Coord) => coord.rotateRight.rotateRight.rotateRight.rotateRight == coord)
  }

  test("rotateLeft four times is identity") {
    check((coord: Coord) =>
      coord.rotateLeft.rotateLeft.rotateLeft.rotateLeft == coord
    )
  }

  test("rotateRight and rotateLeft are inverses") {
    check((coord: Coord) => coord.rotateRight.rotateLeft == coord)
  }

  test("rotateRight turns right") {
    check((dir: Direction) => dir.unitVector.rotateRight == dir.turnRight.unitVector)
  }

  test("rotateLeft turns left") {
    check((dir: Direction) => dir.unitVector.rotateLeft == dir.turnLeft.unitVector)
  }

  test("rotateRight around center four times is identity") {
    check((coord: Coord, about: Coord) =>
      coord.rotateRight(about).rotateRight(about).rotateRight(about).rotateRight(about) == coord
    )
  }

  test("rotateRight around origin is same as rotateRight") {
    check((coord: Coord) => coord.rotateRight(Coord.origin) == coord.rotateRight)
  }

  test("to[Int] is identity") {
    check((coord: Coord) => coord.to[Int] == coord)
  }

  test("to[Long] and back is identity") {
    check((coord: Coord) => coord.to[Long].to[Int] == coord)
  }

  test("to[Long] preserves coordinates") {
    check((coord: Coord) => coord.to[Long] == Point[Long](coord.x.toLong, coord.y.toLong))
  }
