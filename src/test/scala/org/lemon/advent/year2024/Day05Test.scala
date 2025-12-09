package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day05.*

class Day05Test extends UnitTest:

  test("part 1 example") {
    val in = """|47|53
                |97|13
                |97|61
                |97|47
                |75|29
                |61|13
                |75|53
                |29|13
                |97|29
                |53|29
                |61|53
                |97|53
                |61|29
                |47|13
                |75|47
                |97|75
                |47|61
                |75|61
                |47|29
                |75|13
                |53|13
                |
                |75,47,61,53,29
                |97,61,53,29,13
                |75,29,13
                |75,97,47,61,53
                |61,13,29
                |97,13,75,29,47""".stripMargin
    part1(in) shouldBe 143
  }

  test("part 1") {
    part1(read(file(2024)(5))) shouldBe 5275
  }

  test("part 2 example") {
    val in = """|47|53
                |97|13
                |97|61
                |97|47
                |75|29
                |61|13
                |75|53
                |29|13
                |97|29
                |53|29
                |61|53
                |97|53
                |61|29
                |47|13
                |75|47
                |97|75
                |47|61
                |75|61
                |47|29
                |75|13
                |53|13
                |
                |75,47,61,53,29
                |97,61,53,29,13
                |75,29,13
                |75,97,47,61,53
                |61,13,29
                |97,13,75,29,47""".stripMargin
    part2(in) shouldBe 123
  }

  test("part 2") {
    part2(read(file(2024)(5))) shouldBe 6191
  }
