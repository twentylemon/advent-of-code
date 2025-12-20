package org.lemon.advent.year2015

import org.lemon.advent.lib.*
import scala.collection.mutable

private object Day10:

  def lookSay(str: String) =
    @annotation.tailrec
    def loop(idx: Int, acc: mutable.StringBuilder): String =
      if idx >= str.size then acc.result
      else
        val ch = str(idx)
        val length = str.segmentLength(_ == ch, idx)
        loop(idx + length, acc.append(length).append(ch))
    loop(0, mutable.StringBuilder(2 * str.size))

  def part1(input: String, times: Int = 40) =
    Iterator.iterate(input.trim)(lookSay).nth(times).size

  def part2(input: String) =
    Iterator.iterate(input.trim)(lookSay).nth(50).size
