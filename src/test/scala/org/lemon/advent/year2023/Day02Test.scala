package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day02.*

class Day02Test extends UnitTest:

  test("part 1 example") {
    val in = """|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin
    part1(in) shouldBe 8
  }

  test("part 1") {
    part1(read(file(2023)(2))) shouldBe 2449
  }

  test("part 2 example") {
    val in = """|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
                |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
                |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
                |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
                |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin
    part2(in) shouldBe 2286
  }

  test("part 2") {
    part2(read(file(2023)(2))) shouldBe 63981
  }
