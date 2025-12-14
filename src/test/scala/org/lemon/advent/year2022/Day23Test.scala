package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.year2022.Day23.*

class Day23Test extends UnitTest:

  test("simulate example") {
    val in = """|.....
                |..##.
                |..#..
                |.....
                |..##.
                |.....""".stripMargin

    simulate(parse(in)).drop(2).next should contain theSameElementsAs Set(
      Coord(2, 0),
      Coord(4, 1),
      Coord(0, 2),
      Coord(4, 3),
      Coord(2, 5)
    )
  }

  test("part 1 example") {
    val in = """|....#..
                |..###.#
                |#...#.#
                |.#...##
                |#.###..
                |##.#.##
                |.#..#..""".stripMargin

    part1(in) shouldBe 110
  }

  test("part 1") {
    part1(read(file(2022)(23))) shouldBe 4005
  }

  test("part 2 example") {
    val in = """|....#..
                |..###.#
                |#...#.#
                |.#...##
                |#.###..
                |##.#.##
                |.#..#..""".stripMargin

    part2(in) shouldBe 20
  }

  test("part 2") {
    part2(read(file(2022)(23))) shouldBe 1008
  }
