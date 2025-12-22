package org.lemon.advent.year2015

private object Day20:

  def sumOfDivisors(n: Int, limit: Int) =
    @annotation.tailrec
    def loop(d: Int, sum: Int): Int =
      if d * d > n then sum
      else if n % d != 0 then loop(d + 1, sum)
      else
        val hi = n / d
        loop(d + 1, sum + (if hi <= limit then d else 0) + (if d <= limit && d * d != n then hi else 0))
    loop(1, 0)

  def part1(input: String) =
    val n = input.trim.toInt
    Iterator.from(1).find(10 * sumOfDivisors(_, Int.MaxValue) >= n).get

  def part2(input: String) =
    val n = input.trim.toInt
    Iterator.from(1).find(11 * sumOfDivisors(_, 50) >= n).get
