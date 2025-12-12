package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day06.*

class Day06Test extends UnitTest:

  test("part 1 example") {
    part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 7
    part1("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 5
    part1("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 6
    part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 10
    part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 11
  }

  test("part 1") {
    part1(read(file(2022)(6))) shouldBe 1892
  }

  test("part 2 example") {
    part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 19
    part2("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 23
    part2("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 23
    part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 29
    part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 26
  }

  test("part 2") {
    part2(read(file(2022)(6))) shouldBe 2313
  }
