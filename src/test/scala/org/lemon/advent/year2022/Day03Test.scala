package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day03.*

class Day03Test extends UnitTest:

  test("part 1 example") {
    val in = """|vJrwpWtwJgWrhcsFMMfFFhFp
                |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                |PmmdzqPrVvPwwTWBwg
                |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                |ttgJtRGJQctTZtZT
                |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin
    part1(in) shouldBe 157
  }

  test("part 1") {
    part1(read(file(2022)(3))) shouldBe 7990
  }

  test("part 2 example") {
    val in = """|vJrwpWtwJgWrhcsFMMfFFhFp
                |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                |PmmdzqPrVvPwwTWBwg
                |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                |ttgJtRGJQctTZtZT
                |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin
    part2(in) shouldBe 70
  }

  test("part 2") {
    part2(read(file(2022)(3))) shouldBe 2602
  }
