package org.lemon.advent.year2022

private object Day25:

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

  def part1(in: String): Snafu = in.linesIterator.map(s => Snafu(s)).reduce(_ + _)
