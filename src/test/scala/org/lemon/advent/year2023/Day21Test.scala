package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day21._

class Day21Test extends UnitTest:

  val in = """|...........
              |.....###.#.
              |.###.##..#.
              |..#.#...#..
              |....#.#....
              |.##..S####.
              |.##..#...#.
              |.......##..
              |.##.#.####.
              |.##..##.##.
              |...........
              |""".stripMargin

  test("part 1 example") {
    part1(in, depth = 6) shouldBe 16
  }

  test("part 1") {
    part1(read(file(2023)(21))) shouldBe 3682
  }

  test("part 2") {
    part2(read(file(2023)(21))) shouldBe 609012263058042L
  }
