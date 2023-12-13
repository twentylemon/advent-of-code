package org.lemon.advent.year2023

private object Day01:
  def parse(input: String) = input.linesIterator

  def part1(input: String) =
    input.linesIterator.map(_.filter(_.isDigit)).map(n => s"${n.head}${n.takeRight(1)}").map(_.toInt).sum

  val nums = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  ) ++ (1 to 9).map(x => (x.toString, x)).toMap

  def find(line: String): (Int, Int) =
    val min = nums.map((k, v) => (line.indexOf(k), k)).filterKeys(_ != -1)
    val max = nums.map((k, v) => (line.lastIndexOf(k), k)).filterKeys(_ != -1)
    (nums(min(min.keySet.min)), nums(max(max.keySet.max)))

  def part2(input: String) = input.linesIterator
    .map(find)
    .map((x, y) => 10 * x + y)
    .sum
