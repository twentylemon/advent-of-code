package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day11:

  def valid(password: String) =
    def iol = password.exists("iol".contains)
    def straight = password.sliding(3).exists(s => s(0) + 1 == s(1) && s(1) + 1 == s(2))
    def pairPairs =
      @annotation.tailrec
      def loop(str: String, pairs: Set[Char]): Boolean =
        if pairs.size >= 2 then true
        else if str.size < 2 then false
        else if str(0) == str(1) then loop(str.drop(2), pairs + str(0))
        else loop(str.tail, pairs)
      loop(password, Set.empty)
    !iol && pairPairs && straight

  def increment(password: String) =
    @annotation.tailrec
    def loop(str: List[Char], acc: List[Char]): List[Char] =
      str match
        case Nil => 'a' :: acc
        case 'z' :: tail => loop(tail, 'a' :: acc)
        case ch :: tail => tail.reverse ::: (ch + 1).toChar :: acc
    val next = loop(password.reverse.toList, List.empty).mkString
    val iol = next.indexWhere("iol".contains)
    if iol < 0 then next
    else next.take(iol) + (next(iol) + 1).toChar + "a" * (next.size - iol - 1)

  def part1(input: String) =
    Iterator.iterate(input.trim)(increment).drop(1).filter(valid).nth(0)

  def part2(input: String) =
    Iterator.iterate(input.trim)(increment).drop(1).filter(valid).nth(1)
