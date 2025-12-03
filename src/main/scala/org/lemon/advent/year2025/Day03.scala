package org.lemon.advent.year2025

private object Day03:

  def parse(input: String) =
    input.linesIterator.toSeq

  def joltMe(bank: String, batteries: Int) =
    @annotation.tailrec
    def loop(bank: String, batteries: Int, accum: Long): Long =
      if batteries == 0 then accum
      else
        val first = bank.substring(0, bank.length - batteries + 1).max
        val where = bank.indexOf(first)
        loop(bank.substring(where + 1), batteries - 1, accum * 10 + first.asDigit)
    loop(bank, batteries, 0)

  def part1(input: String) =
    val banks = parse(input)
    banks.map(joltMe(_, 2)).sum

  def part2(input: String) =
    val banks = parse(input)
    banks.map(joltMe(_, 12)).sum
