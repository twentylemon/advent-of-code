package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day12.*

class Day12Test extends UnitTest:

  for
    (input, expected) <- Seq(
      "[1,2,3]" -> 6,
      """{"a":2,"b":4}""" -> 6,
      "[[[3]]]" -> 3,
      """{"a":{"b":4},"c":-1}""" -> 3,
      """{"a":[-1,1]}""" -> 0,
      """[-1,{"a":1}]""" -> 0,
      "[]" -> 0,
      "{}" -> 0,
    )
  do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2015)(12))) shouldBe 119433
  }

  for
    (input, expected) <- Seq(
      "[1,2,3]" -> 6,
      """[1,{"c":"red","b":2},3]""" -> 4,
      """{"d":"red","e":[1,2,3,4],"f":5}""" -> 0,
      """[1,"red",5]""" -> 6,
    )
  do
    test(s"part 2 example $input") {
      part2(input) shouldBe expected
    }

  test("part 2") {
    part2(read(file(2015)(12))) shouldBe 68466
  }
