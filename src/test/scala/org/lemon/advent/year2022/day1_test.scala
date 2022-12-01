package org.lemon.advent.year2022

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.io.Source


private def read(input: String) = Source.fromString(input).getLines.toSeq

private def groupByElf(input: Seq[String]): Seq[Seq[String]] =
    input match
        case Seq() => Seq.empty[Seq[String]]
        case seq => Seq(seq.takeWhile(!_.isBlank())) ++
            groupByElf(seq.dropWhile(!_.isBlank()).dropWhile(_.isBlank()))

private def parseCarry(elves: Seq[Seq[String]]): Seq[Int] =
    elves.map(_.map(_.toInt).sum)

private def getMax(elves: Seq[Int]) = elves.max

def getSexiestElf =
    read andThen groupByElf andThen parseCarry andThen getMax

class Day1Test extends AnyFunSuite with Matchers {
    test("day 1 example") {
        val exampleInput = """
            |1000
            |2000
            |3000
            |
            |4000
            |
            |5000
            |6000
            |
            |7000
            |8000
            |9000
            |
            |10000
            """.stripMargin.stripLeading

        getSexiestElf(exampleInput) shouldBe 24000
    }

    test("day 1 part 1") {
        getSexiestElf(Source.fromResource("year2022/day1.txt").mkString) shouldBe 69795
    }

    test("day 1 part 2") {
        val in = Source.fromResource("year2022/day1.txt").mkString
        val carry = (read andThen groupByElf andThen parseCarry)(in)
        carry.sorted.takeRight(3).sum shouldBe 208437
    }
}
