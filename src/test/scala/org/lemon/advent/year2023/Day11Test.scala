package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day11.*

class Day11Test extends UnitTest:

  val in = """|...#......
              |.......#..
              |#.........
              |..........
              |......#...
              |.#........
              |.........#
              |..........
              |.......#..
              |#...#.....""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 374
  }

  test("part 1") {
    part1(read(file(2023)(11))) shouldBe 9509330
  }

  test("part 2 example 10 dist") {
    part2(in, 10) shouldBe 1030
  }

  test("part 2 example 100 dist") {
    part2(in, 100) shouldBe 8410
  }

  test("part 2") {
    part2(read(file(2023)(11)), 1_000_000) shouldBe 635832237682L
  }
