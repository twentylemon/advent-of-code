package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day06.*

class Day06Test extends UnitTest:

  test("part 1 example") {
    val in = """|....#.....
                |.........#
                |..........
                |..#.......
                |.......#..
                |..........
                |.#..^.....
                |........#.
                |#.........
                |......#...""".stripMargin
    part1(in) shouldBe 41
  }

  test("part 1") {
    part1(read(file(2024)(6))) shouldBe 4826
  }

  test("part 2 example") {
    val in = """|....#.....
                |.........#
                |..........
                |..#.......
                |.......#..
                |..........
                |.#..^.....
                |........#.
                |#.........
                |......#...""".stripMargin
    part2(in) shouldBe 6
  }

  test("part 2") {
    part2(read(file(2024)(6))) shouldBe 1721
  }
