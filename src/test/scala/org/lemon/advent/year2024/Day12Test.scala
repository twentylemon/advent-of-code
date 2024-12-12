package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day12._

class Day12Test extends UnitTest:

  test("part 1 example") {
    val in = """|RRRRIICCFF
                |RRRRIICCCF
                |VVRRRCCFFF
                |VVRCCCJFFF
                |VVVVCJJCFE
                |VVIVCCJJEE
                |VVIIICJJEE
                |MIIIIIJJEE
                |MIIISIJEEE
                |MMMISSJEEE""".stripMargin
    part1(in) shouldBe 1930
  }

  test("part 1") {
    part1(read(file(2024)(12))) shouldBe 1486324
  }

  test("part 2 E") {
    val in = """|EEEEE
                |EXXXX
                |EEEEE
                |EXXXX
                |EEEEE""".stripMargin
    part2(in) shouldBe 236
  }

  test("part 2 hole") {
    val in = """|AAAAAA
                |AAABBA
                |AAABBA
                |ABBAAA
                |ABBAAA
                |AAAAAA""".stripMargin
    part2(in) shouldBe 368
  }

  test("part 2 example") {
    val in = """|RRRRIICCFF
                |RRRRIICCCF
                |VVRRRCCFFF
                |VVRCCCJFFF
                |VVVVCJJCFE
                |VVIVCCJJEE
                |VVIIICJJEE
                |MIIIIIJJEE
                |MIIISIJEEE
                |MMMISSJEEE""".stripMargin
    part2(in) shouldBe 1206
  }

  test("part 2") {
    part2(read(file(2024)(12))) shouldBe 898684
  }
