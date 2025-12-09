package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day17.*

class Day17Test extends UnitTest:

  def exe = org.lemon.advent.year2024.Day17.execute

  test("part 1 small example 1") {
    exe(initialState(a = 0, b = 0, c = 9, program = Seq(2, 6))).registerB shouldBe 1
  }

  test("part 1 small example 2") {
    exe(initialState(a = 10, b = 0, c = 0, program = Seq(5, 0, 5, 1, 5, 4))).output shouldBe Seq(0, 1, 2)
  }

  test("part 1 small example 3") {
    exe(initialState(a = 2024, b = 0, c = 0, program = Seq(0, 1, 5, 4, 3, 0))).output shouldBe
      Seq(4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0)
    exe(initialState(a = 2024, b = 0, c = 0, program = Seq(0, 1, 5, 4, 3, 0))).registerA shouldBe 0
  }

  test("part 1 small example 4") {
    exe(initialState(a = 0, b = 29, c = 0, program = Seq(1, 7))).registerB shouldBe 26
  }

  test("part 1 small example 5") {
    exe(initialState(a = 0, b = 2024, c = 43690, program = Seq(4, 0))).registerB shouldBe 44354
  }

  test("part 1 example") {
    val in = """|Register A: 729
                |Register B: 0
                |Register C: 0
                |
                |Program: 0,1,5,4,3,0""".stripMargin
    part1(in) shouldBe "4,6,3,5,6,3,5,2,1,0"
  }

  test("part 1") {
    part1(read(file(2024)(17))) shouldBe "1,7,6,5,1,0,5,0,7"
  }

  test("part 2 example") {
    val in = """|Register A: 2024
                |Register B: 0
                |Register C: 0
                |
                |Program: 0,3,5,4,3,0""".stripMargin
    part2(in) shouldBe 117440L
  }

  test("part 2") {
    part2(read(file(2024)(17))) shouldBe 236555995274861L
  }
