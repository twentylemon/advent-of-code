package org.lemon.advent.year2022

import org.lemon.UnitTest
import scala.util.Using
import scala.io.Source

class Day03Test extends UnitTest {

    def priority(ch: Char) = if ch.isUpper then ch.toInt - 'A'.toInt + 27 else ch.toInt - 'a'.toInt + 1

    def intersection[T](lhs: Set[T], rhs: Set[T]) = lhs.intersect(rhs)

    def compartmentsOf(sack: String): Seq[Seq[Char]] =
        sack
            .splitAt(sack.length / 2)
            .productIterator
            .map(_.asInstanceOf[String])
            .map(_.toCharArray.toSeq)
            .toSeq

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
        Using.resource(Source.fromResource("year2022/day03.txt"))(source =>
            part1(source.mkString) shouldBe 7990
        )
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
        Using.resource(Source.fromResource("year2022/day03.txt"))(source =>
            part2(source.mkString) shouldBe 2602
        )
    }

}
