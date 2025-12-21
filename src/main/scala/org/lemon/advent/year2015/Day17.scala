package org.lemon.advent.year2015

private object Day17:

  def parse(input: String) = input.linesIterator.map(_.toInt).toIndexedSeq

  def count(containers: Seq[Int], target: Int) =
    def loop(idx: Int, remaining: Int, used: Int): Seq[Int] = remaining match
      case 0 => Seq(used)
      case r if r < 0 || !containers.isDefinedAt(idx) => Seq.empty
      case r => loop(idx + 1, r - containers(idx), used + 1) ++ loop(idx + 1, r, used)
    loop(0, target, 0)

  def part1(input: String, target: Int = 150) =
    val containers = parse(input)
    count(containers, target).size

  def part2(input: String, target: Int = 150) =
    val containers = parse(input)
    val ways = count(containers, target)
    val min = ways.min
    ways.count(_ == min)
