package org.lemon.advent.year2015

private object Day08:

  def decode(str: String) =
    @annotation.tailrec
    def loop(str: List[Char], acc: Int): Int = str match
      case Nil => acc
      case '\\' :: 'x' :: a :: b :: tail => loop(tail, acc + 1)
      case '\\' :: a :: tail => loop(tail, acc + 1)
      case '"' :: tail => loop(tail, acc)
      case _ :: tail => loop(tail, acc + 1)
    loop(str.toList, 0)

  def part1(input: String) =
    input.linesIterator.map(str => str.size - decode(str)).sum

  def encode(str: String) =
    @annotation.tailrec
    def loop(str: List[Char], acc: Int): Int = str match
      case Nil => acc
      case '\\' :: tail => loop(tail, acc + 2)
      case '"' :: tail => loop(tail, acc + 2)
      case _ :: tail => loop(tail, acc + 1)
    loop(str.toList, 2)

  def part2(input: String) =
    input.linesIterator.map(str => encode(str) - str.size).sum
