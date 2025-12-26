package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day17.*

class Day17Test extends UnitTest:

  for
    (input, expected) <- Seq(
      "ihgpwlah" -> "DDRRRD",
      "kglvqrro" -> "DDUDRLRRUDRD",
      "ulqzkmiv" -> "DRURDRUDDLLDLUURRDULRLDUUDDDRR",
    )
  do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2016)(17))) shouldBe "RDULRDDRRD"
  }

  for
    (input, expected) <- Seq(
      "ihgpwlah" -> 370,
      "kglvqrro" -> 492,
      "ulqzkmiv" -> 830,
    )
  do
    test(s"part 2 example $input") {
      part2(input) shouldBe expected
    }

  test("part 2") {
    part2(read(file(2016)(17))) shouldBe 752
  }
