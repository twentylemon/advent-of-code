package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day18.*

class Day18Test extends UnitTest:

  val in = """|R 6 (#70c710)
              |D 5 (#0dc571)
              |L 2 (#5713f0)
              |D 2 (#d2c081)
              |R 2 (#59c680)
              |D 2 (#411b91)
              |L 5 (#8ceee2)
              |U 2 (#caa173)
              |L 1 (#1b58a2)
              |U 2 (#caa171)
              |R 2 (#7807d2)
              |U 3 (#a77fa3)
              |L 2 (#015232)
              |U 2 (#7a21e3)""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 62
  }

  test("part 1") {
    part1(read(file(2023)(18))) shouldBe 50465
  }

  test("part 2 example") {
    part2(in) shouldBe 952408144115L
  }

  test("part 2") {
    part2(read(file(2023)(18))) shouldBe 82712746433310L
  }
