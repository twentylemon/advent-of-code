package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day05.*

class Day05Test extends UnitTest:

  test("part 1 example") {
    val in = """|ugknbfddgicrmopn
                |aaa
                |jchzalrnumimnmhp
                |haegwjzuvuyypxyu
                |dvszwmarrgswjxmb
                |""".stripMargin
    part1(in) shouldBe 1 + 1 + 0 + 0 + 0
  }

  test("part 1") {
    part1(read(file(2015)(5))) shouldBe 238
  }

  test("part 2 example") {
    val in = """|qjhvhtzxzqqjkmpb
                |xxyxx
                |uurcxstgmygtbstg
                |ieodomkazucvgmuy
                |""".stripMargin
    part2(in) shouldBe 1 + 1 + 0 + 0
  }

  test("part 2") {
    part2(read(file(2015)(5))) shouldBe 69
  }
