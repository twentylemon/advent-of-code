package org.lemon.advent.year2022

import org.lemon.advent._

class Day03Test extends UnitTest {

  def priority(ch: Char) = if ch.isUpper then ch.toInt - 'A'.toInt + 27 else ch.toInt - 'a'.toInt + 1

  def compartmentsOf(sack: String): Seq[Seq[Char]] =
    sack
      .splitAt(sack.length / 2)
      .toList
      .map(_.toCharArray.toSeq)

  def getPriorityOfCommon(compartments: Seq[Seq[Char]]) = compartments
    .map(_.toSet)
    .reduce(_ intersect _)
    .map(priority)
    .sum

  def part1(input: String) = input.linesIterator.map(compartmentsOf).map(getPriorityOfCommon).sum

  def part2(input: String) = input.linesIterator
    .map(_.toCharArray.toSeq)
    .grouped(3)
    .map(getPriorityOfCommon)
    .sum

  test("part 1 example") {
    val input = """
            |vJrwpWtwJgWrhcsFMMfFFhFp
            |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
            |PmmdzqPrVvPwwTWBwg
            |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
            |ttgJtRGJQctTZtZT
            |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.strip

    part1(input) shouldBe 157
  }

  test("part 1") {
    part1(read(file(2022)(3))) shouldBe 7990
  }

  test("part 2 example") {
    val input = """
            |vJrwpWtwJgWrhcsFMMfFFhFp
            |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
            |PmmdzqPrVvPwwTWBwg
            |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
            |ttgJtRGJQctTZtZT
            |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.strip

    part2(input) shouldBe 70
  }

  test("part 2") {
    part2(read(file(2022)(3))) shouldBe 2602
  }

}
