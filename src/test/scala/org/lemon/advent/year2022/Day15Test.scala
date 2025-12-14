package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day15.*

class Day15Test extends UnitTest:

  test("part 1 example") {
    val in = """|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

    part1(in, 10) shouldBe 26
  }

  test("part 1") {
    part1(read(file(2022)(15)), 2000000) shouldBe 5607466
  }

  test("part 2 example") {
    val in = """|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

    part2(in, 0 to 20) shouldBe 56000011L
  }

  test("part 2") {
    part2(read(file(2022)(15)), 0 to 4000000) shouldBe 12543202766584L
  }
