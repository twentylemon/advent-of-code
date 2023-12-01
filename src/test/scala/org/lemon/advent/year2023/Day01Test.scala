package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day01._

class Day01Test extends UnitTest:

  test("part 1 example") {
    val in = """|1abc2
                |pqr3stu8vwx
                |a1b2c3d4e5f
                |treb7uchet""".stripMargin
    part1(in) shouldBe 142
  }

  test("part 1") {
    part1(read(file(2023)(1))) shouldBe 54081
  }

  test("part 2 example") {
    val in = """|two1nine
                |eightwothree
                |abcone2threexyz
                |xtwone3four
                |4nineeightseven2
                |zoneight234
                |7pqrstsixteen""".stripMargin
    part2(in) shouldBe 281
  }

  test("part 2") {
    part2(read(file(2023)(1))) shouldBe 54649
  }
