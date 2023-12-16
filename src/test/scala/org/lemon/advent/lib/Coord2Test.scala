package org.lemon.advent.lib

import org.scalacheck._
import org.lemon.advent._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.lemon.advent.lib.Coord2._

class Coord2Test extends UnitTest:

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

  test("manhattan distance of adjacent is 1") {
    check((coord: Coord) => coord.adjacent.forall(_.manhattan(coord) == 1))
  }

  test("manhattan distance of surrounding is 1 or 2") {
    check((coord: Coord) => coord.adjacent.map(_ manhattan coord).forall(x => x == 1 || x == 2))
  }

  test("surrounding contains all adjacent") {
    check((coord: Coord) => coord.adjacent.toSet.subsetOf(coord.surrounding.toSet))
  }
