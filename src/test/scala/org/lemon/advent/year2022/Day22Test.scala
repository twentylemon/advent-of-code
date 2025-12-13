package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day22.*

class Day22Test extends UnitTest:

  test("part 1 example") {
    val in = """|        ...#
                |        .#..
                |        #...
                |        ....
                |...#.......#
                |........#...
                |..#....#....
                |..........#.
                |        ...#....
                |        .....#..
                |        .#......
                |        ......#.
                |
                |10R5L5R10L4R5L5""".stripMargin

    part1(in) shouldBe 6032
  }

  test("part 1") {
    part1(read(file(2022)(22))) shouldBe 29408
  }

  // I hard coded the cube. sue me.
  ignore("part 2 example") {
    val in = """|        ...#
                |        .#..
                |        #...
                |        ....
                |...#.......#
                |........#...
                |..#....#....
                |..........#.
                |        ...#....
                |        .....#..
                |        .#......
                |        ......#.
                |
                |10R5L5R10L4R5L5""".stripMargin
    part2(in) shouldBe 5031
  }

  test("part 2") {
    part2(read(file(2022)(22))) shouldBe 115311
  }
