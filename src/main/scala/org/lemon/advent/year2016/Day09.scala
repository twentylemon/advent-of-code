package org.lemon.advent.year2016

private object Day09:

  def inflate(zip: String, goDeeper: Boolean): Long =
    zip match
      case s"$pre(${len}x$n)$suf" =>
        val (data, rest) = suf.splitAt(len.toInt)
        val dataSize = if goDeeper then inflate(data, goDeeper) else data.size.toLong
        pre.size + dataSize * n.toInt + inflate(rest, goDeeper)
      case other => other.size

  def part1(input: String) =
    input.linesIterator.map(inflate(_, goDeeper = false)).sum

  def part2(input: String) =
    input.linesIterator.map(inflate(_, goDeeper = true)).sum
