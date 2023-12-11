package org.lemon.advent

import org.scalacheck._
import org.lemon.advent.Coord2._

class Coord2Test extends UnitTest:

  test("left and right are opposites", Lib) {
    check((coord: Coord) => coord == coord.left.right)
  }

  test("up and down are opposites", Lib) {
    check((coord: Coord) => coord == coord.up.down)
  }

  test("manhattan distance of adjacent is 1", Lib) {
    check((coord: Coord) => coord.adjacent.forall(_.manhattan(coord) == 1))
  }

  test("manhattan distance of surrounding is 1 or 2", Lib) {
    check((coord: Coord) => coord.adjacent.map(_ manhattan coord).forall(x => x == 1 || x == 2))
  }

  test("surrounding contains all adjacent", Lib) {
    check((coord: Coord) => coord.adjacent.toSet.subsetOf(coord.surrounding.toSet))
  }
