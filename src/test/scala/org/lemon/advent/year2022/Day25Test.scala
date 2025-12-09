package org.lemon.advent.year2022

import org.lemon.advent.*

class Day25Test extends UnitTest {

  object Snafus:
    opaque type Snafu = String
    given Conversion[String, Snafu] = s => Snafu(s)
    object Snafu:
      def apply(value: String): Snafu = value
      val digitToDec = Map('0' -> 0, '1' -> 1, '2' -> 2, '-' -> -1, '=' -> -2)
      val decToDigit = digitToDec.map(_.swap).toMap

    extension (snafu: Snafu)
      def +(rhs: Snafu): Snafu =
        val result = snafu.reverse.zipAll(rhs.reverse, '0', '0')
          .foldLeft(("", 0))((accum, digits) =>
            val (digit, carry) = addDigit(digits._1, digits._2, accum._2)
            (accum._1 + digit, carry)
          )
        if result._2 != 0 then (result._1 + Snafu.decToDigit(result._2)).reverse
        else if result._1.forall(_ == '0') then "0"
        else result._1.reverse.dropWhile(_ == '0')

      private def addDigit(lhs: Char, rhs: Char, carry: Int): (Char, Int) =
        Snafu.digitToDec(lhs) + Snafu.digitToDec(rhs) + carry match
          case x if x > 2 => (Snafu.decToDigit(x - 5), 1)
          case x if x < -2 => (Snafu.decToDigit(x + 5), -1)
          case x => (Snafu.decToDigit(x), 0)
  import Snafus.*

  def part1(in: Seq[String]): Snafu = in.map(s => Snafu(s)).reduce(_ + _)

  for
    (lhs, rhs, expected) <- Seq[(Snafu, Snafu, Snafu)](
      ("0", "0", "0"), // 0 + 0 = 0
      ("1", "1", "2"), // 1 + 1 = 2
      ("2", "2", "1-"), // 2 + 2 = 4
      ("-", "-", "="), // -1 + -1 = -2
      ("=", "=", "-1"), // -2 + -2 = -4
      ("=", "==", "-21"), // -2 + -12 = -14
      ("2", "22", "1=-"), // 2 + 12 = 14
      ("1=", "1=", "11"), // 3 + 3 = 6
      ("1=", "=", "1"), // 3 + -2 = 1
      ("2-", "2=", "1=2"), // 9 + 8 = 17
      ("22", "22", "10-"), // 12 + 12 = 24
      ("10", "1", "11"), // 5 + 1 = 6
      ("1-", "=", "2"), // 4 + -2 = 2
      ("2=", "2=", "1=1"), // 8 + 8 = 16
      ("1==", "1==", "101"), // 13 + 13 = 26
    )
  do test(s"snafu `$lhs + $rhs = $expected") {
    (lhs + rhs) shouldBe expected
  }

  test("part 1 example") {
    val in = """|1=-0-2
                |12111
                |2=0=
                |21
                |2=01
                |111
                |20012
                |112
                |1=-1=
                |1-12
                |12
                |1=
                |122""".stripMargin

    part1(in.linesIterator.toSeq) shouldBe Snafu("2=-1=0")
  }

  test("part 1") {
    part1(readLines(file(2022)(25))) shouldBe Snafu("2=0=02-0----2-=02-10")
  }
}
