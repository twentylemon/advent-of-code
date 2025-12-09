package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day05.*

class Day05Test extends UnitTest:
  
  val in = """|seeds: 79 14 55 13
              |
              |seed-to-soil map:
              |50 98 2
              |52 50 48
              |
              |soil-to-fertilizer map:
              |0 15 37
              |37 52 2
              |39 0 15
              |
              |fertilizer-to-water map:
              |49 53 8
              |0 11 42
              |42 0 7
              |57 7 4
              |
              |water-to-light map:
              |88 18 7
              |18 25 70
              |
              |light-to-temperature map:
              |45 77 23
              |81 45 19
              |68 64 13
              |
              |temperature-to-humidity map:
              |0 69 1
              |1 0 69
              |
              |humidity-to-location map:
              |60 56 37
              |56 93 41""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 35
  }

  test("part 1") {
    part1(read(file(2023)(5))) shouldBe 379811651L
  }

  test("part 2 example") {
    part2(in) shouldBe 46
  }

  test("part 2") {
    part2(read(file(2023)(5))) shouldBe 27992443
  }
