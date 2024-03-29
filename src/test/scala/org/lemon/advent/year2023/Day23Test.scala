package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day23._

class Day23Test extends UnitTest:

  val in = """|#.#####################
              |#.......#########...###
              |#######.#########.#.###
              |###.....#.>.>.###.#.###
              |###v#####.#v#.###.#.###
              |###.>...#.#.#.....#...#
              |###v###.#.#.#########.#
              |###...#.#.#.......#...#
              |#####.#.#.#######.#.###
              |#.....#.#.#.......#...#
              |#.#####.#.#.#########v#
              |#.#...#...#...###...>.#
              |#.#.#v#######v###.###v#
              |#...#.>.#...>.>.#.###.#
              |#####v#.#.###v#.#.###.#
              |#.....#...#...#.#.#...#
              |#.#########.###.#.#.###
              |#...###...#...#...#.###
              |###.###.#.###v#####v###
              |#...#...#.#.>.>.#.>.###
              |#.###.###.#.###.#.#v###
              |#.....###...###...#...#
              |#####################.#
              |""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 94
  }

  test("part 1") {
    part1(read(file(2023)(23))) shouldBe 2218
  }

  test("part 2 example") {
    part2(in) shouldBe 154
  }

  test("part 2") {
    part2(read(file(2023)(23))) shouldBe 6674
  }
