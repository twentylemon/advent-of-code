package org.lemon.advent.year2025

import org.lemon.advent.*
import org.lemon.advent.year2025.Day11.*

class Day11Test extends UnitTest:

  test("part 1 example") {
    val in = """|aaa: you hhh
                |you: bbb ccc
                |bbb: ddd eee
                |ccc: ddd eee fff
                |ddd: ggg
                |eee: out
                |fff: out
                |ggg: out
                |hhh: ccc fff iii
                |iii: out
                |""".stripMargin
    part1(in) shouldBe 5
  }

  test("part 1") {
    part1(read(file(2025)(11))) shouldBe 508
  }

  test("part 2 example") {
    val in = """|svr: aaa bbb
                |aaa: fft
                |fft: ccc
                |bbb: tty
                |tty: ccc
                |ccc: ddd eee
                |ddd: hub
                |hub: fff
                |eee: dac
                |dac: fff
                |fff: ggg hhh
                |ggg: out
                |hhh: out
                |""".stripMargin
    part2(in) shouldBe 2
  }

  test("part 2") {
    part2(read(file(2025)(11))) shouldBe 315116216513280L
  }
