package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day14.*

class Day14Test extends UnitTest:

  test("part 1 example") {
    val in = """|Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
                |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
                |""".stripMargin
    part1(in, at = 1000) shouldBe 1120
  }

  test("part 1") {
    part1(read(file(2015)(14))) shouldBe 2660
  }

  test("part 2 example") {
    val in = """|Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
                |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
                |""".stripMargin
    part2(in, at = 1000) shouldBe 689
  }

  test("part 2") {
    part2(read(file(2015)(14))) shouldBe 1256
  }
