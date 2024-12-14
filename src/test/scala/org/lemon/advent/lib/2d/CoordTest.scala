package org.lemon.advent.lib.`2d`

import org.lemon.advent._
import org.lemon.advent.lib.`2d`.Coord._
import org.lemon.advent.lib.`2d`._
import org.scalacheck.Prop._
import org.scalacheck._

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

  test("manhattan distance of surrounding is 1 or 2") {
    check((coord: Coord) => coord.adjacent.map(_ manhattan coord).forall(x => x == 1 || x == 2))
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

  test("shiftInto returns a coord in the area") {
    check((area: Area, coord: Coord) => area.contains(coord.shiftInto(area)))
  }
