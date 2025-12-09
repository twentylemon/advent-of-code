package org.lemon.advent.year2025

import org.lemon.advent.*
import org.lemon.advent.year2025.Day01.*

class Day01Test extends UnitTest:

  test("part 1 example") {
    val in = """|L68
                |L30
                |R48
                |L5
                |R60
                |L55
                |L1
                |L99
                |R14
                |L82
                |""".stripMargin
    part1(in) shouldBe 3
  }

  test("part 1") {
    part1(read(file(2025)(1))) shouldBe 1100
  }

  test("part 2 example") {
    val in = """|L68
                |L30
                |R48
                |L5
                |R60
                |L55
                |L1
                |L99
                |R14
                |L82
                |""".stripMargin
    part2(in) shouldBe 6
  }

  test("part 2") {
    part2(read(file(2025)(1))) shouldBe 6358
  }
