package org.lemon.advent.year2016

private object Day16:

  def fillDisk(initial: String, length: Int) =
    Iterator.iterate(initial)(str => str + "0" + str.reverse.map(c => if c == '0' then '1' else '0'))
      .find(_.size >= length)
      .map(_.take(length))
      .get

  def checksum(data: String) =
    Iterator.iterate(data)(_.grouped(2).map(s => if s(0) == s(1) then '1' else '0').mkString)
      .find(_.size % 2 != 0)
      .get

  def part1(input: String, length: Int = 272) =
    checksum(fillDisk(input.trim, length))

  def part2(input: String) =
    part1(input, length = 35651584)
