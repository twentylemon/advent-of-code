package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day08.*

class Day08Test extends UnitTest:

  test("part 1 example 1") {
    val in = """|RL
                |
                |AAA = (BBB, CCC)
                |BBB = (DDD, EEE)
                |CCC = (ZZZ, GGG)
                |DDD = (DDD, DDD)
                |EEE = (EEE, EEE)
                |GGG = (GGG, GGG)
                |ZZZ = (ZZZ, ZZZ)""".stripMargin
    part1(in) shouldBe 2
  }

  test("part 1 example 2") {
    val in = """|LLR
                |
                |AAA = (BBB, BBB)
                |BBB = (AAA, ZZZ)
                |ZZZ = (ZZZ, ZZZ)""".stripMargin
    part1(in) shouldBe 6
  }

  test("part 1") {
    part1(read(file(2023)(8))) shouldBe 19199
  }

  test("part 2 example") {
    val in = """|LR
                |
                |11A = (11B, XXX)
                |11B = (XXX, 11Z)
                |11Z = (11B, XXX)
                |22A = (22B, XXX)
                |22B = (22C, 22C)
                |22C = (22Z, 22Z)
                |22Z = (22B, 22B)
                |XXX = (XXX, XXX)""".stripMargin
    part2(in) shouldBe 6
  }

  test("part 2") {
    part2(read(file(2023)(8))) shouldBe 13663968099527L
  }

  test("part 2 alternate example") {
    val in = """|LR
                |
                |11A = (11B, XXX)
                |11B = (XXX, 11Z)
                |11Z = (11B, XXX)
                |22A = (22B, XXX)
                |22B = (22C, 22C)
                |22C = (22Z, 22Z)
                |22Z = (22B, 22B)
                |XXX = (XXX, XXX)""".stripMargin
    part2_alternate(in) shouldBe 6
  }

  test("part 2 alternate") {
    part2_alternate(read(file(2023)(8))) shouldBe 13663968099527L
  }
