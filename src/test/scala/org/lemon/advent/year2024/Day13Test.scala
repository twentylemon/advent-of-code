package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day13.*

class Day13Test extends UnitTest:

  test("part 1 example") {
    val in = """|Button A: X+94, Y+34
                |Button B: X+22, Y+67
                |Prize: X=8400, Y=5400
                |
                |Button A: X+26, Y+66
                |Button B: X+67, Y+21
                |Prize: X=12748, Y=12176
                |
                |Button A: X+17, Y+86
                |Button B: X+84, Y+37
                |Prize: X=7870, Y=6450
                |
                |Button A: X+69, Y+23
                |Button B: X+27, Y+71
                |Prize: X=18641, Y=10279""".stripMargin
    part1(in) shouldBe 480
  }

  test("part 1") {
    part1(read(file(2024)(13))) shouldBe 29436
  }

  test("part 2") {
    part2(read(file(2024)(13))) shouldBe 103729094227877L
  }
