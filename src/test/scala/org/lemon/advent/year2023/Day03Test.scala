package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day03.*

class Day03Test extends UnitTest:

  test("part 1 example") {
    val in = """|467..114..
                |...*......
                |..35..633.
                |......#...
                |617*......
                |.....+.58.
                |..592.....
                |......755.
                |...$.*....
                |.664.598..""".stripMargin
    part1(in) shouldBe 4361
  }

  test("part 1") {
    part1(read(file(2023)(3))) shouldBe 532445
  }

  test("part 2 example") {
    val in = """|467..114..
                |...*......
                |..35..633.
                |......#...
                |617*......
                |.....+.58.
                |..592.....
                |......755.
                |...$.*....
                |.664.598..""".stripMargin
    part2(in) shouldBe 467835
  }

  test("part 2") {
    part2(read(file(2023)(3))) shouldBe 79842967
  }
